//! TODO: explain in detail how this works.
//!
//! A few notes that will be helpful. Structs are encoded differently depending
//! on whether they appear as a variant in an enum; if so, the tag is included.
//! This allows the alignment of the struct to take the tag into account.

extern crate proc_macro;
#[macro_use]
extern crate quote;

use std::collections::HashSet;
use std::fmt::Write;
use std::ops::Deref;

use proc_macro::TokenStream;
use syn::{parse::Parse, parse::ParseStream, parse_macro_input, spanned::Spanned};
use syn::{
    Data, Expr, ExprLit, Fields, FieldsNamed, FieldsUnnamed, GenericArgument, ItemEnum, ItemStruct,
    Lit, PathArguments, TypeArray, TypePath,
};

#[derive(Clone, Copy, PartialEq)]
enum GpuScalar {
    I8,
    I16,
    I32,
    F32,
    U8,
    U16,
    U32,
}

enum GpuType {
    Scalar(GpuScalar),
    Vector(GpuScalar, usize),
    /// Used mostly for the body of enum variants.
    InlineStruct(String),
    Ref(Box<GpuType>),
}

struct GpuEnum {
    name: String,
    variants: Vec<(String, Vec<GpuType>)>,
}

enum GpuTypeDef {
    Struct(String, Vec<(String, GpuType)>),
    Enum(GpuEnum),
}

struct GpuModule {
    name: String,
    /// Set of item names that are used as enum variants.
    enum_variants: HashSet<String>,
    defs: Vec<GpuTypeDef>,
}

impl GpuScalar {
    fn metal_typename(self) -> &'static str {
        match self {
            GpuScalar::F32 => "float",
            GpuScalar::I8 => "char",
            GpuScalar::I16 => "short",
            GpuScalar::I32 => "int",
            GpuScalar::U8 => "uchar",
            GpuScalar::U16 => "ushort",
            GpuScalar::U32 => "uint",
        }
    }

    fn size(self) -> usize {
        match self {
            GpuScalar::F32 => 4,
            GpuScalar::I8 => 1,
            GpuScalar::I16 => 2,
            GpuScalar::I32 => 4,
            GpuScalar::U8 => 1,
            GpuScalar::U16 => 2,
            GpuScalar::U32 => 4,
        }
    }

    fn from_syn(ty: &syn::Type) -> Option<Self> {
        ty_as_single_ident(ty).and_then(|ident| match ident.as_str() {
            "f32" => Some(GpuScalar::F32),
            "i8" => Some(GpuScalar::I8),
            "i16" => Some(GpuScalar::I16),
            "i32" => Some(GpuScalar::I32),
            "u8" => Some(GpuScalar::U8),
            "u16" => Some(GpuScalar::U16),
            "u32" => Some(GpuScalar::U32),
            _ => None,
        })
    }
}

impl GpuType {
    fn metal_typename(&self) -> String {
        match self {
            GpuType::Scalar(scalar) => scalar.metal_typename().into(),
            GpuType::Vector(scalar, size) => format!("{}{}", scalar.metal_typename(), size),
            GpuType::InlineStruct(name) => format!("{}Packed", name),
            // TODO: probably want to have more friendly names for simple struct refs.
            GpuType::Ref(inner) => {
                if let GpuType::InlineStruct(name) = inner.deref() {
                    format!("{}Ref", name)
                } else {
                    "uint".into()
                }
            }
        }
    }

    fn size(&self, module: &GpuModule) -> usize {
        match self {
            GpuType::Scalar(scalar) => scalar.size(),
            GpuType::Vector(scalar, size) => scalar.size() * size,
            GpuType::InlineStruct(name) => module.resolve_by_name(&name).unwrap().size(module),
            GpuType::Ref(_name) => 4,
        }
    }

    fn alignment(&self, module: &GpuModule) -> usize {
        // TODO: there are alignment problems with vectors of 3
        match self {
            GpuType::Scalar(scalar) => scalar.size(),
            GpuType::Vector(scalar, size) => scalar.size() * size,
            GpuType::InlineStruct(name) => module.resolve_by_name(&name).unwrap().alignment(module),
            GpuType::Ref(_name) => 4,
        }
    }

    /// Report whether type is a scalar or simple vector
    fn is_small(&self) -> bool {
        match self {
            GpuType::Scalar(_) => true,
            GpuType::Vector(_, _) => true,
            GpuType::InlineStruct(_) => false,
            GpuType::Ref(_) => true,
        }
    }

    fn from_syn(ty: &syn::Type) -> Result<Self, String> {
        //println!("gputype {:#?}", ty);
        if let Some(scalar) = GpuScalar::from_syn(ty) {
            return Ok(GpuType::Scalar(scalar));
        }
        if let Some(name) = ty_as_single_ident(ty) {
            // Note: we're not doing any validation here.
            return Ok(GpuType::InlineStruct(name));
        }
        match ty {
            syn::Type::Path(TypePath {
                path: syn::Path { segments, .. },
                ..
            }) => {
                if segments.len() == 1 {
                    let seg = &segments[0];
                    if seg.ident == "Ref" {
                        if let PathArguments::AngleBracketed(args) = &seg.arguments {
                            if args.args.len() == 1 {
                                if let GenericArgument::Type(inner) = &args.args[0] {
                                    let inner_ty = GpuType::from_syn(inner)?;
                                    return Ok(GpuType::Ref(Box::new(inner_ty)));
                                }
                            }
                        }
                    }
                }
                Err("unknown path case".into())
            }
            syn::Type::Array(TypeArray { elem, len, .. }) => {
                if let Some(elem) = GpuScalar::from_syn(&elem) {
                    if let Some(len) = expr_int_lit(len) {
                        // maybe sanity-check length here
                        Ok(GpuType::Vector(elem, len))
                    } else {
                        Err("can't deal with variable length scalar arrays".into())
                    }
                } else {
                    Err("can't deal with non-scalar arrays".into())
                }
            }
            _ => Err("unknown type".into()),
        }
    }
}

impl GpuTypeDef {
    fn from_syn(item: &syn::Item) -> Result<Self, String> {
        match item {
            syn::Item::Struct(ItemStruct {
                ident,
                fields: Fields::Named(FieldsNamed { named, .. }),
                ..
            }) => {
                let mut fields = Vec::new();
                for field in named {
                    let field_ty = GpuType::from_syn(&field.ty)?;
                    let field_name = field.ident.as_ref().ok_or("need name".to_string())?;
                    fields.push((field_name.to_string(), field_ty));
                }
                Ok(GpuTypeDef::Struct(ident.to_string(), fields))
            }
            syn::Item::Enum(ItemEnum {
                ident, variants, ..
            }) => {
                let mut v = Vec::new();
                for variant in variants {
                    let vname = variant.ident.to_string();
                    let mut fields = Vec::new();
                    if let Fields::Unnamed(FieldsUnnamed { unnamed, .. }) = &variant.fields {
                        for field in unnamed {
                            fields.push(GpuType::from_syn(&field.ty)?);
                        }
                    }
                    v.push((vname, fields));
                }
                let en = GpuEnum {
                    name: ident.to_string(),
                    variants: v,
                };
                Ok(GpuTypeDef::Enum(en))
            }
            _ => {
                eprintln!("{:#?}", item);
                Err("unknown item".into())
            }
        }
    }

    fn name(&self) -> &str {
        match self {
            GpuTypeDef::Struct(name, _) => &name,
            GpuTypeDef::Enum(en) => &en.name,
        }
    }

    fn collect_refs(&self, enum_variants: &mut HashSet<String>) {
        if let GpuTypeDef::Enum(en) = self {
            for variant in &en.variants {
                if let Some(GpuType::InlineStruct(name)) = variant.1.first() {
                    enum_variants.insert(name.clone());
                }
            }
        }
    }

    /// Size of the body of the definition.
    fn size(&self, module: &GpuModule) -> usize {
        match self {
            GpuTypeDef::Struct(name, fields) => {
                let mut offset = 0;
                if module.enum_variants.contains(name) {
                    offset += 4;
                }
                for (_name, field) in fields {
                    offset += align_padding(offset, field.alignment(module));
                    offset += field.size(module);
                }
                offset
            }
            GpuTypeDef::Enum(en) => {
                let mut max_offset = 4;
                for (_name, fields) in &en.variants {
                    let mut offset = 4;
                    for field in fields {
                        if let GpuType::InlineStruct(_) = field {
                            if offset == 4 {
                                offset = 0;
                            }
                        }
                        // Alignment needs work :/
                        //offset += align_padding(offset, field.alignment(module));
                        offset += field.size(module);
                    }
                    max_offset = max_offset.max(offset);
                }
                max_offset
            }
        }
    }

    /// Alignment of the body of the definition.
    fn alignment(&self, module: &GpuModule) -> usize {
        match self {
            GpuTypeDef::Struct(name, fields) => {
                let mut alignment = 1;
                if module.enum_variants.contains(name) {
                    alignment = 4;
                }
                for (_name, field) in fields {
                    alignment = alignment.max(field.alignment(module));
                }
                alignment
            }
            GpuTypeDef::Enum(_en) => unimplemented!(),
        }
    }

    fn to_metal(&self, module: &GpuModule) -> String {
        let mut r = String::new();
        match self {
            GpuTypeDef::Struct(name, fields) => {
                let rn = format!("{}Ref", name);
                // The packed struct definition (is missing variable sized arrays)
                write!(r, "struct {}Packed {{\n", name).unwrap();
                if module.enum_variants.contains(name) {
                    write!(r, "    uint tag;\n").unwrap();
                }
                for (fieldname, ty) in fields {
                    write!(r, "    {} {};\n", ty.metal_typename(), fieldname).unwrap();
                }
                write!(r, "}};\n").unwrap();
                // Read of packed structure
                write!(
                    r,
                    "{}Packed {}_read(const device char *buf, {} ref) {{\n",
                    name, name, rn
                )
                .unwrap();
                write!(
                    r,
                    "    return *((const device {}Packed *)(buf + ref));\n",
                    name
                )
                .unwrap();
                write!(r, "}}\n").unwrap();
                // Unpacked field accessors
                for (fieldname, ty) in fields {
                    if ty.is_small() {
                        let tn = ty.metal_typename();
                        write!(
                            r,
                            "{} {}_{}(const device char *buf, {} ref) {{\n",
                            tn, name, fieldname, rn
                        )
                        .unwrap();
                        write!(
                            r,
                            "    return ((const device {}Packed *)(buf + ref))->{};\n",
                            name, fieldname
                        )
                        .unwrap();
                        write!(r, "}}\n").unwrap();
                    }
                }
            }
            GpuTypeDef::Enum(en) => {
                let rn = format!("{}Ref", en.name);
                write!(r, "struct {} {{\n", en.name).unwrap();
                write!(r, "    uint tag;\n").unwrap();
                let size = self.size(module);
                let body_size = ((size + 3) >> 2) - 1;
                write!(r, "    uint body[{}];\n", body_size).unwrap();
                write!(r, "}};\n").unwrap();
                write!(
                    r,
                    "uint {}_tag(const device char *buf, {} ref) {{\n",
                    en.name, rn
                )
                .unwrap();
                write!(
                    r,
                    "    return ((const device {} *)(buf + ref))->tag;\n",
                    en.name
                )
                .unwrap();
                write!(r, "}}\n").unwrap();
                // TODO: current code base is 1-based, but we could switch to 0
                let mut tag = 1;
                for (name, _fields) in &en.variants {
                    write!(r, "#define {}_{} {}\n", en.name, name, tag).unwrap();
                    tag += 1;
                }
            }
        }
        r
    }

    fn to_metal_wr(&self, _module: &GpuModule) -> String {
        let mut r = String::new();
        match self {
            GpuTypeDef::Struct(name, _fields) => {
                // Write of packed structure
                write!(
                    r,
                    "void {}_write(device char *buf, {}Ref ref, {}Packed s) {{\n",
                    name, name, name
                )
                .unwrap();
                write!(
                    r,
                    "    *((device {}Packed *)(buf + ref)) = s;\n",
                    name
                )
                .unwrap();
                write!(r, "}}\n").unwrap();
            }
            // We don't write enum structs, we only write their variants.
            _ => (),
        }
        r
    }
}

impl GpuModule {
    fn from_syn(module: &syn::ItemMod) -> Result<Self, String> {
        let name = module.ident.to_string();
        let mut defs = Vec::new();
        let mut enum_variants = HashSet::new();
        if let Some((_brace, items)) = &module.content {
            for item in items {
                let def = GpuTypeDef::from_syn(item)?;
                def.collect_refs(&mut enum_variants);
                defs.push(def);
            }
        }
        Ok(GpuModule {
            name,
            enum_variants,
            defs,
        })
    }

    fn resolve_by_name(&self, name: &str) -> Option<&GpuTypeDef> {
        for def in &self.defs {
            if def.name() == name {
                return Some(&def);
            }
        }
        None
    }

    fn to_metal(&self) -> String {
        let mut r = String::new();
        for def in &self.defs {
            write!(&mut r, "typedef uint {}Ref;\n", def.name()).unwrap();
        }
        for def in &self.defs {
            r.push_str(&def.to_metal(self));
        }
        r
    }

    fn to_metal_wr(&self) -> String {
        let mut r = String::new();
        for def in &self.defs {
            r.push_str(&def.to_metal_wr(self));
        }
        r
    }
}

// Probably don't need this, will use ItemMod instead.
#[derive(Debug)]
struct Items(Vec<syn::Item>);

fn ty_as_single_ident(ty: &syn::Type) -> Option<String> {
    if let syn::Type::Path(TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        if segments.len() == 1 {
            let seg = &segments[0];
            if seg.arguments == PathArguments::None {
                return Some(seg.ident.to_string());
            }
        }
    }
    None
}

fn expr_int_lit(e: &Expr) -> Option<usize> {
    if let Expr::Lit(ExprLit {
        lit: Lit::Int(lit_int),
        ..
    }) = e
    {
        lit_int.base10_parse().ok()
    } else {
        None
    }
}

fn align_padding(offset: usize, alignment: usize) -> usize {
    offset.wrapping_neg() & (alignment - 1)
}

#[proc_macro]
pub fn piet_metal(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::ItemMod);
    //println!("input: {:#?}", input);
    let module = GpuModule::from_syn(&input).unwrap();
    let gen_metal_fn = format_ident!("gen_metal_{}", input.ident);
    let result = module.to_metal();
    let result_wr = module.to_metal_wr();
    let expanded = quote! {
        fn #gen_metal_fn(gpu_write: bool) {
            println!("{}", #result);
            if gpu_write {
                println!("{}", #result_wr);
            }
        }
    };
    expanded.into()
}

#[proc_macro_derive(PietMetal)]
pub fn derive_piet_metal(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    derive_proc_metal_impl(input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn derive_proc_metal_impl(input: syn::DeriveInput) -> Result<proc_macro2::TokenStream, syn::Error> {
    println!("input: {:#?}", input);
    match &input.data {
        Data::Struct { .. } => {
            println!("it's a struct!");
        }
        _ => (),
    }
    let s = "this is a string";
    let expanded = quote! {
        fn foo() {
            println!("this was generated by proc macro: {}", #s);
        }
    };
    Ok(expanded)
}

impl Parse for Items {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let mut items = Vec::new();
        while !input.is_empty() {
            items.push(input.parse()?)
        }
        Ok(Items(items))
    }
}
