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

#[derive(Clone)]
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

    fn hlsl_typename(self) -> &'static str {
        match self {
            GpuScalar::F32 => "float",
            GpuScalar::I32 => "int",
            GpuScalar::U32 => "uint",
            // everything else is stored in a uint (ignoring F64 for now)
            _ => "uint",
        }
    }

    fn size(self) -> usize {
        match self {
            GpuScalar::F32 | GpuScalar::I32 | GpuScalar::U32 => 4,
            GpuScalar::I8 | GpuScalar::U8 => 1,
            GpuScalar::I16 | GpuScalar::U16 => 2,
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

impl std::fmt::Display for GpuScalar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GpuScalar::F32 => write!(f, "F32"),
            GpuScalar::I8 => write!(f, "I8"),
            GpuScalar::I16 => write!(f, "I16"),
            GpuScalar::I32 => write!(f, "I32"),
            GpuScalar::U8 => write!(f, "U8"),
            GpuScalar::U16 => write!(f, "U16"),
            GpuScalar::U32 => write!(f, "U32"),
        }
    }
}

// HLSL helpers
fn hlsl_get_ref_plus_offset(current_offset: usize) -> String {
    match current_offset {
        0 => String::from("ref"),
        _ => format!("ref + {}", current_offset),
    }
}

fn hlsl_calculate_num_uints_required_for_storage_of_vector(
    ty: &GpuScalar,
    num_elements: usize,
) -> usize {
    let num_scalars_in_a_uint: usize = match ty {
        GpuScalar::I8 | GpuScalar::U8 => 4,
        GpuScalar::I16 | GpuScalar::U16 => 2,
        _ => 1,
    };

    (num_elements + num_scalars_in_a_uint - 1) / num_scalars_in_a_uint
}

fn hlsl_generate_value_extractor(value_bit_size: u32) -> String {
    if value_bit_size > 31 {
        panic!("nonsensical to generate an extractor for a value with bit size greater than 31");
    }
    let mut extractor: String = String::new();

    let mask_width: usize = 2_usize.pow(value_bit_size) - 1;

    write!(
        extractor,
        "inline uint extract_{}bit_value(uint bit_shift, uint package) {{\n",
        value_bit_size
    )
    .unwrap();
    write!(extractor, "    uint mask = {};\n", mask_width).unwrap();
    write!(
        extractor,
        "{}",
        "    uint result = (package >> bit_shift) & mask;\n\n    return result;\n}\n\n"
    )
    .unwrap();

    extractor
}

fn hlsl_generate_field_reader(
    package_type: &GpuType,
    package_fieldname: &str,
    current_offset: usize,
) -> String {
    let type_name = package_type.hlsl_typename();
    match package_type {
        GpuType::Scalar(_) => format!(
            "    {} {} = buf.Load({});\n",
            type_name,
            package_fieldname,
            hlsl_get_ref_plus_offset(current_offset),
        ),
        GpuType::Vector(scalar, size) => match size {
            0 => panic!("vector of size 0 is not well defined!"),
            1 => format!(
                "    {}{} {} = buf.Load({});\n",
                scalar.hlsl_typename(),
                size,
                package_fieldname,
                hlsl_get_ref_plus_offset(current_offset)
            ),
            _ => format!(
                "    {}{} {} = buf.Load{}({});\n",
                scalar.hlsl_typename(),
                size,
                package_fieldname,
                size,
                hlsl_get_ref_plus_offset(current_offset)
            ),
        },
        GpuType::InlineStruct(isn) => format!(
            "    {} {} = {}_read({});\n",
            isn,
            package_fieldname,
            isn,
            hlsl_get_ref_plus_offset(current_offset)
        ),
        GpuType::Ref(inner) => {
            if let GpuType::InlineStruct(isn) = inner.deref() {
                format!(
                    "    {}Ref {} = buf.Load({});\n",
                    isn,
                    package_fieldname,
                    hlsl_get_ref_plus_offset(current_offset),
                )
            } else {
                format!(
                    "    uint {} = buf.Load({});\n",
                    package_fieldname,
                    hlsl_get_ref_plus_offset(current_offset),
                )
            }
        }
    }
}

fn hlsl_generate_field_accessor(
    type_string: &str,
    struct_name: &str,
    package_fieldname: &str,
    ref_name: &str,
    reader: &str,
) -> String {
    let mut field_accessor = String::new();

    write!(
        field_accessor,
        "inline {} {}_{}(ByteAddressBuffer buf, {} ref) {{\n",
        type_string, struct_name, package_fieldname, ref_name,
    )
    .unwrap();
    write!(field_accessor, "{}", reader).unwrap();
    write!(field_accessor, "    return {};\n}}\n\n", package_fieldname).unwrap();

    field_accessor
}

fn hlsl_generate_unpacker(
    struct_name: &str,
    package_fieldname: &str,
    packed_fields: &Vec<PackedField>,
) -> String {
    let mut unpacker = String::new();

    if packed_fields.len() > 0 {
        for pf in packed_fields {
            if !pf.ty.is_small() {
                continue;
            }
            let (unpacked_fieldname, unpacked_type, offset_in_package) =
                (&pf.name, &pf.ty, pf.offset_in_package);
            match unpacked_type {
                GpuType::Scalar(scalar) => {
                    let size_in_bits = 8 * scalar.size();
                    let hlsl_type: String = match scalar {
                        GpuScalar::U8 | GpuScalar::U16 => String::from("uint"),
                        GpuScalar::I8 | GpuScalar::I16 => String::from("int"),
                        _ => panic!("unexpected unpacking of 32 bit value!"),
                    };

                    write!(
                        unpacker,
                        "inline uint {}_unpack_{}(uint {}) {{\n    {} result;\n\n",
                        struct_name, unpacked_fieldname, package_fieldname, hlsl_type,
                    )
                    .unwrap();
                    write!(
                        unpacker,
                        "    result = extract_{}bit_value({}, {});\n",
                        size_in_bits, offset_in_package, package_fieldname
                    )
                    .unwrap();
                }
                GpuType::Vector(scalar, unpacked_size) => {
                    let scalar_size_in_bits = 8 * scalar.size();
                    let hlsl_type: String = match scalar {
                        GpuScalar::U8 | GpuScalar::U16 => String::from("uint"),
                        GpuScalar::I8 | GpuScalar::I16 => String::from("int"),
                        _ => panic!("unexpected unpacking of 32 bit value!"),
                    };

                    let num_uints_required_for_storage =
                        hlsl_calculate_num_uints_required_for_storage_of_vector(
                            &scalar,
                            *unpacked_size,
                        );
                    write!(
                        unpacker,
                        "inline uint{} {}_unpack_{}(uint{} {}) {{\n    {}{} result;\n\n",
                        unpacked_size,
                        struct_name,
                        unpacked_fieldname,
                        num_uints_required_for_storage,
                        package_fieldname,
                        hlsl_type,
                        unpacked_size,
                    )
                    .unwrap();

                    for i in 0..*unpacked_size {
                        write!(
                            unpacker,
                            "    result[{}] = extract_{}bit_value({}, {});\n",
                            i,
                            scalar_size_in_bits,
                            32 - (i + 1) * scalar_size_in_bits,
                            package_fieldname
                        )
                        .unwrap();
                    }
                }
                _ => panic!(
                    "only expected small types, got: {}",
                    unpacked_type.hlsl_typename()
                ),
            }
            write!(unpacker, "    return result;\n").unwrap();
            write!(unpacker, "}}\n\n").unwrap();
        }
    }

    unpacker
}

fn hlsl_generate_structure_reading_functions(
    name: &String,
    package_fields: &Vec<PackagedField>,
    module: &GpuModule,
) -> String {
    let mut structure_decoder = String::new();
    let mut field_accessors: Vec<String> = Vec::new();
    let mut unpackers: Vec<String> = Vec::new();

    let ref_name = format!("{}Ref", name);

    write!(
        structure_decoder,
        "inline {}Packed {}_read(ByteAddressBuffer buf, {} ref) {{\n",
        name, name, ref_name,
    )
    .unwrap();
    write!(structure_decoder, "    {}Packed result;\n\n", name,).unwrap();

    let mut current_offset: usize = 0;
    if module.enum_variants.contains(name) {
        // account for tag
        current_offset = 4;
    }

    for package_field in package_fields {
        let package_type = package_field.ty.as_ref().unwrap();
        let reader: String =
            hlsl_generate_field_reader(&package_type, &package_field.name, current_offset);

        field_accessors.push(hlsl_generate_field_accessor(
            &package_type.hlsl_typename(),
            name,
            &package_field.name,
            &ref_name,
            &reader,
        ));

        if package_type.is_small() {
            unpackers.push(hlsl_generate_unpacker(
                name,
                &package_field.name,
                &package_field.packed_fields,
            ));
        }

        write!(structure_decoder, "{}", reader).unwrap();
        write!(
            structure_decoder,
            "    result.{} = {};\n\n",
            package_field.name, package_field.name
        )
        .unwrap();

        current_offset += package_type.size(module);
    }

    write!(structure_decoder, "    return result;\n}}\n\n",).unwrap();

    for field_accessor in field_accessors {
        write!(structure_decoder, "{}", field_accessor).unwrap();
    }

    for unpacker in unpackers {
        write!(structure_decoder, "{}", unpacker).unwrap();
    }

    structure_decoder
}

#[derive(Clone)]
struct PackedField {
    name: String,
    ty: GpuType,
    offset_in_package: usize,
}

#[derive(Clone)]
struct PackagedField {
    name: String,
    ty: Option<GpuType>,
    packed_fields: Vec<PackedField>,
    size: usize,
}

enum PackagedFieldExtensionResult {
    SuccessAndOpen,
    SuccessAndFinished,
    FailAndFinished,
}

impl PackagedField {
    fn new() -> PackagedField {
        PackagedField {
            name: String::new(),
            ty: None,
            size: 0,
            packed_fields: vec![],
        }
    }

    fn extend(
        &mut self,
        module: &GpuModule,
        fieldtype: &GpuType,
        fieldname: &str,
    ) -> Result<PackagedFieldExtensionResult, String> {
        if !self.is_finished() {
            let field_size = fieldtype.size(module);

            if !(field_size + self.size < 4) {
                if self.is_empty() {
                    self.packed_fields.push(PackedField {
                        name: String::from(fieldname),
                        ty: fieldtype.clone(),
                        offset_in_package: 0,
                    });
                    self.finish(module).unwrap();
                    Ok(PackagedFieldExtensionResult::SuccessAndFinished)
                } else {
                    self.finish(module).unwrap();
                    Ok(PackagedFieldExtensionResult::FailAndFinished)
                }
            } else {
                self.packed_fields.push(PackedField {
                    name: String::from(fieldname),
                    ty: fieldtype.clone(),
                    offset_in_package: 32 - self.size * 8,
                });
                self.size += field_size;
                Ok(PackagedFieldExtensionResult::SuccessAndOpen)
            }
        } else {
            Err(String::from("cannot extend finished package"))
        }
    }

    fn is_empty(&self) -> bool {
        self.packed_fields.len() == 0
    }

    fn is_finished(&self) -> bool {
        self.ty.is_some()
    }

    fn finish(&mut self, module: &GpuModule) -> Result<(), String> {
        if !self.is_finished() {
            if self.is_empty() {
                Err(String::from("cannot finish empty package"))
            } else {
                let packed_field_names = self
                    .packed_fields
                    .iter()
                    .map(|pf| pf.name.clone())
                    .collect::<Vec<String>>();
                self.name = packed_field_names.join("_");

                self.ty = match self.packed_fields.len() {
                    0 => {
                        let pfty = &self.packed_fields[0].ty;
                        match pfty {
                            GpuType::Scalar(scalar) => match scalar {
                                GpuScalar::F32 | GpuScalar::I32 | GpuScalar::U32 => {
                                    Ok(Some(pfty.clone()))
                                }
                                _ => Ok(Some(GpuType::Scalar(GpuScalar::U32))),
                            },
                            GpuType::Vector(scalar, size) => match scalar {
                                GpuScalar::F32 | GpuScalar::I32 | GpuScalar::U32 => {
                                    Ok(Some(pfty.clone()))
                                }
                                _ => Ok(Some(GpuType::Vector(
                                    GpuScalar::U32,
                                    hlsl_calculate_num_uints_required_for_storage_of_vector(
                                        scalar, *size,
                                    ),
                                ))),
                            },
                            GpuType::InlineStruct(_) => {
                                let num_uints_required_for_storage =
                                    (pfty.size(module) + 4 - 1) / 4;
                                match num_uints_required_for_storage {
                                    0 => Err(String::from("encountered struct of size 0")),
                                    1 => Ok(Some(GpuType::Scalar(GpuScalar::U32))),
                                    2 | 3 | 4 => Ok(Some(GpuType::Vector(
                                        GpuScalar::U32,
                                        num_uints_required_for_storage,
                                    ))),
                                    _ => Err(String::from(
                                        "struct requires more than 8 bytes to store",
                                    )),
                                }
                            }
                            GpuType::Ref(inner) => {
                                if let GpuType::InlineStruct(_) = inner.deref() {
                                    Ok(Some(pfty.clone()))
                                } else {
                                    Ok(Some(GpuType::Scalar(GpuScalar::U32)))
                                }
                            }
                        }
                    }
                    _ => match self.packed_fields.iter().any(|pf| pf.ty.size(module) == 32) {
                        true => Err(String::from(
                            "cannot pack multiple types along with at least one 32 bit sized type",
                        )),
                        false => {
                            let summed_size: usize =
                                self.packed_fields.iter().map(|pf| pf.ty.size(module)).sum();
                            let num_uints_required_for_storage = (summed_size + 4 - 1) / 4;
                            match num_uints_required_for_storage {
                                0 => Err(String::from("encountered struct of size 0")),
                                1 => Ok(Some(GpuType::Scalar(GpuScalar::U32))),
                                2 | 3 | 4 => Ok(Some(GpuType::Vector(
                                    GpuScalar::U32,
                                    num_uints_required_for_storage,
                                ))),
                                _ => Err(String::from(
                                    "packed fields require more than 8 bytes to store",
                                )),
                            }
                        }
                    },
                }?;
                Ok(())
            }
        } else {
            Err(String::from("cannot finish finished package"))
        }
    }
}

fn hlsl_generate_packaged_fields(
    module: &GpuModule,
    fields: &Vec<(String, GpuType)>,
) -> Vec<PackagedField> {
    let mut package_fields: Vec<PackagedField> = Vec::new();

    let mut current_packaged_field = PackagedField::new();
    for (fieldname, ty) in fields {
        match current_packaged_field
            .extend(module, &ty, &fieldname)
            .unwrap()
        {
            PackagedFieldExtensionResult::SuccessAndFinished => {
                package_fields.push(current_packaged_field.clone());
                current_packaged_field = PackagedField::new();
            }
            PackagedFieldExtensionResult::FailAndFinished => {
                package_fields.push(current_packaged_field.clone());
                current_packaged_field = PackagedField::new();
                current_packaged_field
                    .extend(module, &ty, &fieldname)
                    .unwrap();
            }
            _ => {}
        }
    }

    if !current_packaged_field.is_finished() {
        if !current_packaged_field.is_empty() {
            current_packaged_field.finish(module);
            package_fields.push(current_packaged_field.clone());
        }
    }

    package_fields
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

    fn hlsl_typename(&self) -> String {
        match self {
            GpuType::Scalar(scalar) => scalar.hlsl_typename().into(),
            GpuType::Vector(scalar, size) => {
                match scalar {
                    GpuScalar::F32 | GpuScalar::I32 | GpuScalar::U32 => {
                        format!("{}{}", scalar.hlsl_typename(), size)
                    }
                    _ => {
                        let num_uints_required_for_storage =
                            hlsl_calculate_num_uints_required_for_storage_of_vector(scalar, *size);
                        //TODO: where should sanity checks for size be done?
                        if num_uints_required_for_storage == 1 {
                            String::from("uint")
                        } else {
                            format!("uint{}", num_uints_required_for_storage)
                        }
                    }
                }
            }
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
                    let fieldname = field.ident.as_ref().ok_or("need name".to_string())?;
                    fields.push((fieldname.to_string(), field_ty));
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

    fn to_hlsl(&self, module: &GpuModule) -> String {
        let mut r = String::new();
        match self {
            GpuTypeDef::Struct(name, fields) => {
                // The packed struct definition (is missing variable sized arrays)
                write!(r, "struct {}Packed {{\n", name).unwrap();
                if module.enum_variants.contains(name) {
                    write!(r, "    uint tag;\n").unwrap();
                }
                let package_fields = hlsl_generate_packaged_fields(module, fields);
                for package_field in package_fields.iter() {
                    write!(
                        r,
                        "    {} {};\n",
                        package_field.ty.as_ref().unwrap().hlsl_typename(),
                        package_field.name
                    )
                    .unwrap()
                }
                write!(r, "{}", "}\n").unwrap();

                write!(
                    r,
                    "{}",
                    hlsl_generate_structure_reading_functions(name, &package_fields, module)
                )
                .unwrap();
            }
            GpuTypeDef::Enum(en) => {
                let rn = format!("{}Ref", en.name);

                write!(r, "struct {} {{\n", en.name).unwrap();
                write!(r, "    uint tag;\n").unwrap();

                let size = self.size(module);
                // TODO: this sometimes predicts incorrect number of u32s needed to store body (differences with metal alignment)
                let body_size = ((size + 3) >> 2) - 1;

                write!(r, "    uint body[{}];\n", body_size).unwrap();
                write!(r, "}};\n").unwrap();
                write!(
                    r,
                    "inline uint {}_tag(ByteAddressBuffer buf, {} ref) {{\n",
                    en.name, rn
                )
                .unwrap();

                write!(r, "    uint result = buf.Load(ref);\n    return result;\n").unwrap();
                write!(r, "}}\n\n").unwrap();
            }
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

    fn to_hlsl(&self) -> String {
        let mut r = String::new();

        write!(&mut r, "{}", hlsl_generate_value_extractor(8)).unwrap();
        write!(&mut r, "{}", hlsl_generate_value_extractor(16)).unwrap();

        for def in &self.defs {
            write!(&mut r, "typedef uint {}Ref;\n", def.name()).unwrap();
        }

        write!(&mut r, "\n").unwrap();
        for def in &self.defs {
            r.push_str(&def.to_hlsl(self));
        }

        for def in &self.defs {
            let name = def.name();
            if !(self.enum_variants.contains(name)) {
                write!(
                    r,
                    "#define {}_SIZE {}\n",
                    to_snake_case(name).to_uppercase(),
                    def.size(self)
                )
                .unwrap();
            }
            if let GpuTypeDef::Enum(en) = def {
                let mut tag: usize = 0;
                for (name, _fields) in &en.variants {
                    write!(r, "#define {}_{} {}\n", en.name, name, tag).unwrap();
                    tag += 1;
                }
            }
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
    let expanded = quote! {
        fn #gen_metal_fn() {
            println!("{}", #result);
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

#[proc_macro]
pub fn piet_hlsl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::ItemMod);
    //println!("input: {:#?}", input);
    let module = GpuModule::from_syn(&input).unwrap();
    let gen_hlsl_fn = format_ident!("gen_hlsl_{}", input.ident);
    let result = module.to_hlsl();
    let expanded = quote! {
        fn #gen_hlsl_fn() -> String{
            String::from(#result)
        }
    };
    expanded.into()
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

fn to_snake_case(mut str: &str) -> String {
    let mut words = vec![];
    // Preserve leading underscores
    str = str.trim_start_matches(|c: char| {
        if c == '_' {
            words.push(String::new());
            true
        } else {
            false
        }
    });
    for s in str.split('_') {
        let mut last_upper = false;
        let mut buf = String::new();
        if s.is_empty() {
            continue;
        }
        for ch in s.chars() {
            if !buf.is_empty() && buf != "'" && ch.is_uppercase() && !last_upper {
                words.push(buf);
                buf = String::new();
            }
            last_upper = ch.is_uppercase();
            buf.extend(ch.to_lowercase());
        }
        words.push(buf);
    }
    words.join("_")
}
