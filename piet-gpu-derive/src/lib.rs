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
use syn::parse_macro_input;
use syn::{
    Expr, ExprLit, Fields, FieldsNamed, FieldsUnnamed, GenericArgument, ItemEnum, ItemStruct, Lit,
    PathArguments, TypeArray, TypePath,
};

/// The target shader language. We can't make this a public type because of Rust rules.
#[derive(Copy, Clone, PartialEq)]
enum TargetLang {
    Hlsl,
    Msl,
}

/// A scalar that can be represented in a packed data structure.
#[derive(Clone, Copy, PartialEq)]
enum GpuScalar {
    I8,
    I16,
    I32,
    F32,
    U8,
    U16,
    U32,
    // TODO: Add F16
}

/// An algebraic datatype.
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
    attrs: HashSet<String>,
    /// Set of item names that are used as enum variants.
    enum_variants: HashSet<String>,
    defs: Vec<GpuTypeDef>,
}

impl TargetLang {
    /// The typed function argument for "buf"
    fn buf_arg(self) -> &'static str {
        match self {
            TargetLang::Hlsl => "ByteAddressBuffer buf",
            TargetLang::Msl => "const device char *buf",
        }
    }

    /// The typed function argument for "buf" when it is mutable
    fn mut_buf_arg(self) -> &'static str {
        match self {
            TargetLang::Hlsl => "RWByteAddressBuffer buf",
            TargetLang::Msl => "device char *buf",
        }
    }

    /// An expression for loading a number of uints.
    fn load_expr(self, offset: usize, size: usize) -> String {
        let tail = if offset == 0 {
            "".into()
        } else {
            format!(" + {}", offset)
        };
        let size_str = vector_size_str(size);
        match self {
            TargetLang::Hlsl => format!("buf.Load{}(ref{})", size_str, tail),
            TargetLang::Msl => {
                let packed = if size == 1 { "" } else { "packed_" };
                format!(
                    "*(device const {}uint{}*)(buf + ref{})",
                    packed, size_str, tail
                )
            }
        }
    }
    /// An expression for storing a number of uints.
    ///
    /// Return value is a statement without any surrounding whitespace.
    fn store_stmt(self, offset: usize, size: usize, value: &str) -> String {
        let tail = if offset == 0 {
            "".into()
        } else {
            format!(" + {}", offset)
        };
        let size_str = vector_size_str(size);
        match self {
            TargetLang::Hlsl => format!("buf.Store{}(ref{}, {});", size_str, tail, value),
            TargetLang::Msl => {
                let packed = if size == 1 { "" } else { "packed_" };
                format!(
                    "*(device {}uint{}*)(buf + ref{}) = {};",
                    packed, size_str, tail, value
                )
            }
        }
    }
}

impl GpuScalar {
    /// The unpacked type of the scalar value.
    fn unpacked_type(self, target: TargetLang) -> GpuScalar {
        match target {
            TargetLang::Hlsl => match self {
                GpuScalar::I8 | GpuScalar::I16 => GpuScalar::I32,
                GpuScalar::U8 | GpuScalar::U16 => GpuScalar::U32,
                _ => self,
            },
            _ => self,
        }
    }

    fn typename(self, target: TargetLang) -> &'static str {
        if target == TargetLang::Hlsl && self.size() < 4 {
            panic!(
                "Internal logic error: trying to determine HLSL typename for {} byte value",
                self.size()
            );
        }
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
            GpuScalar::F32 | GpuScalar::I32 | GpuScalar::U32 => 4,
            GpuScalar::I8 | GpuScalar::U8 => 1,
            GpuScalar::I16 | GpuScalar::U16 => 2,
        }
    }

    /// Convert an expression with type "uint" into the given scalar.
    fn cvt(self, inner: &str, target: TargetLang) -> String {
        self.cvt_vec(inner, 1, target)
    }

    /// Convert a uint vector into the given vector
    fn cvt_vec(self, inner: &str, size: usize, target: TargetLang) -> String {
        let size = vector_size_str(size);
        match (target, self) {
            (TargetLang::Hlsl, GpuScalar::F32) => format!("asfloat({})", inner),
            (TargetLang::Hlsl, GpuScalar::I32) => format!("asint({})", inner),
            (TargetLang::Msl, GpuScalar::F32) => format!("as_type<float{}>({})", size, inner),
            (TargetLang::Msl, GpuScalar::I32) => format!("as_type<int{}>({})", size, inner),
            // TODO: need to be smarter about signed int conversion
            _ => inner.into(),
        }
    }

    /// Convert the given vector into a uint vector
    fn cvt_vec_inv(self, inner: &str, size: usize, target: TargetLang) -> String {
        let size = vector_size_str(size);
        match (target, self) {
            (TargetLang::Hlsl, GpuScalar::F32) | (TargetLang::Hlsl, GpuScalar::I32) => {
                format!("asuint({})", inner)
            }
            (TargetLang::Msl, GpuScalar::F32) | (TargetLang::Msl, GpuScalar::I32) => {
                format!("as_type<uint{}>({})", size, inner)
            }
            // TODO: need to be smarter about signed int conversion
            _ => inner.into(),
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

    fn gen_derive(&self) -> proc_macro2::TokenStream {
        match self {
            GpuScalar::F32 => quote!(f32),
            GpuScalar::I8 => quote!(i8),
            GpuScalar::I16 => quote!(i16),
            GpuScalar::I32 => quote!(i32),
            GpuScalar::U8 => quote!(u8),
            GpuScalar::U16 => quote!(u16),
            GpuScalar::U32 => quote!(u32),
        }
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

/// If `c = 0`, return `"var_name`, else `"var_name + c"`
fn simplified_add(var_name: &str, c: usize) -> String {
    if c == 0 {
        String::from(var_name)
    } else {
        format!("{} + {}", var_name, c)
    }
}

/// Suffix to add to scalar type to make it into a vector.
///
/// For size of 1, returns empty string, though "usize1" is usually valid.
/// This is so we have one name for the same type, and also so the suffix
/// can be used for `ByteAddressBuf.Load` method names.
fn vector_size_str(size: usize) -> &'static str {
    match size {
        1 => "",
        2 => "2",
        3 => "3",
        4 => "4",
        _ => panic!("illegal vector size {}", size),
    }
}

/// Return number of `uints` required to store `num_bytes` bytes.
fn size_in_uints(num_bytes: usize) -> usize {
    // a `uint` has a size of 4 bytes, (size_in_bytes + 4 - 1) / 4
    (num_bytes + 3) / 4
}

// TODO: this only generates unsigned extractors, we will need signed as well.
fn generate_value_extractor(size_in_bits: u32) -> String {
    if size_in_bits > 31 {
        panic!("nonsensical to generate an extractor for a value with bit size greater than 31");
    }
    let mut extractor: String = String::new();

    let mask_width: usize = 2_usize.pow(size_in_bits) - 1;

    write!(
        extractor,
        "inline uint extract_{}bit_value(uint bit_shift, uint package) {{\n",
        size_in_bits
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

/// A `PackedField` stores `StoredField`s
#[derive(Clone)]
struct StoredField {
    name: String,
    ty: GpuType,
    /// The offset of the field within the packed field, in bits.
    offset: usize,
}

/// A `PackedStruct` has `PackedField`s
#[derive(Clone)]
struct PackedField {
    name: String,
    /// The type of the package, as stored packed.
    ty: Option<GpuType>,
    stored_fields: Vec<StoredField>,
    size: usize,
}

/// Possible results of the `pack` method on a `PackedField`.
#[derive(PartialEq)]
enum PackResult {
    SuccessAndOpen,
    SuccessAndClosed,
    FailAndClosed,
}

#[derive(Clone)]
struct PackedStruct {
    name: String,
    packed_fields: Vec<PackedField>,
    is_enum_variant: bool,
}

struct SpecifiedStruct {
    name: String,
    fields: Vec<(String, GpuType)>,
    packed_form: PackedStruct,
}

impl StoredField {
    fn generate_unpacker(
        &self,
        packed_struct_name: &str,
        packed_field_name: &str,
        target: TargetLang,
    ) -> String {
        let mut unpacker = String::new();

        // A hack to get the base struct name
        let stripped_name = &packed_struct_name[0..packed_struct_name.len() - 6];
        if self.ty.is_small() {
            match self.ty {
                GpuType::Scalar(scalar) => {
                    let size_in_bits = 8 * scalar.size();
                    let hlsl_typename: String = match scalar {
                        GpuScalar::F32 | GpuScalar::I32 | GpuScalar::U32 => {
                            panic!("unexpected unpacking of 32 bit value!")
                        }
                        _ => String::from(scalar.unpacked_type(target).typename(target)),
                    };

                    write!(
                        unpacker,
                        "inline uint {}_unpack_{}(uint {}) {{\n    {} result;\n\n",
                        stripped_name, self.name, packed_field_name, hlsl_typename,
                    )
                    .unwrap();

                    write!(
                        unpacker,
                        "    result = extract_{}bit_value({}, {});\n",
                        size_in_bits, self.offset, packed_field_name
                    )
                    .unwrap();
                }
                GpuType::Vector(scalar, unpacked_size) => {
                    let scalar_size_in_bits = 8 * scalar.size();
                    let unpacked_typename = self.ty.unpacked_typename(target);

                    let size_in_uints = size_in_uints(&scalar.size() * unpacked_size);
                    write!(
                        unpacker,
                        "inline {} {}_unpack_{}(uint{} {}) {{\n    {} result;\n\n",
                        unpacked_typename,
                        stripped_name,
                        self.name,
                        size_in_uints,
                        packed_field_name,
                        unpacked_typename,
                    )
                    .unwrap();

                    for i in 0..unpacked_size {
                        let subscript = if size_in_uints == 1 {
                            "".into()
                        } else {
                            format!("[{}]", (i * scalar_size_in_bits) / 32)
                        };
                        let extracted = scalar.cvt(
                            &format!(
                                "extract_{}bit_value({}, {}{})",
                                scalar_size_in_bits,
                                self.offset + (i * scalar_size_in_bits) % 32,
                                packed_field_name,
                                subscript
                            ),
                            target,
                        );
                        write!(unpacker, "    result[{}] = {};\n", i, extracted).unwrap();
                    }
                }
                _ => panic!(
                    "only expected small types, got: {}",
                    self.ty.unpacked_typename(target)
                ),
            }

            write!(unpacker, "{}", "    return result;\n").unwrap();
            write!(unpacker, "{}", "}\n\n").unwrap();
        }

        unpacker
    }
}

impl PackedField {
    fn new() -> PackedField {
        PackedField {
            name: String::new(),
            ty: None,
            size: 0,
            stored_fields: vec![],
        }
    }

    fn pack(
        &mut self,
        module: &GpuModule,
        field_type: &GpuType,
        field_name: &str,
    ) -> Result<PackResult, String> {
        if !self.is_closed() {
            let field_size = field_type.size(module);

            if field_size + self.size > 4 {
                if self.is_empty() {
                    self.stored_fields.push(StoredField {
                        name: field_name.into(),
                        ty: field_type.clone(),
                        offset: 0,
                    });
                    self.close(module).unwrap();
                    Ok(PackResult::SuccessAndClosed)
                } else {
                    self.close(module).unwrap();
                    Ok(PackResult::FailAndClosed)
                }
            } else {
                self.stored_fields.push(StoredField {
                    name: String::from(field_name),
                    ty: field_type.clone(),
                    offset: self.size * 8,
                });
                self.size += field_size;
                Ok(PackResult::SuccessAndOpen)
            }
        } else {
            Err("cannot extend closed package".into())
        }
    }

    fn is_empty(&self) -> bool {
        self.stored_fields.is_empty()
    }

    fn is_closed(&self) -> bool {
        self.ty.is_some()
    }

    /// True when the packed and unpacked types differ.
    fn is_packed(&self, struct_result: bool) -> bool {
        if self.stored_fields.len() != 1 {
            return true;
        }
        match self.stored_fields[0].ty {
            GpuType::Scalar(scalar) => scalar.size() < 4,
            GpuType::Vector(scalar, _) => scalar.size() < 4,
            GpuType::InlineStruct(_) => struct_result,
            _ => false,
        }
    }

    fn close(&mut self, module: &GpuModule) -> Result<(), String> {
        if !self.is_closed() {
            if self.is_empty() {
                Err("cannot close empty package".into())
            } else {
                let stored_field_names = self
                    .stored_fields
                    .iter()
                    .map(|pf| pf.name.clone())
                    .collect::<Vec<String>>();
                self.name = stored_field_names.join("_");

                if self.is_packed(false) {
                    let summed_size = self.stored_fields.iter().map(|pf| pf.ty.size(module)).sum();
                    let size_in_uints = size_in_uints(summed_size);
                    if size_in_uints == 1 {
                        self.ty = Some(GpuType::Scalar(GpuScalar::U32));
                    } else {
                        self.ty = Some(GpuType::Vector(GpuScalar::U32, size_in_uints));
                    }
                } else {
                    self.ty = Some(self.stored_fields[0].ty.clone());
                }
                Ok(())
            }
        } else {
            Err("cannot close closed package".into())
        }
    }

    fn generate_reader(&self, current_offset: usize, target: TargetLang) -> Result<String, String> {
        if let Some(ty) = &self.ty {
            let type_name = ty.unpacked_typename(target);
            let packed_field_name = &self.name;

            match ty {
                GpuType::Scalar(scalar) => {
                    let load_expr = target.load_expr(current_offset, 1);
                    let cvt_exp = scalar.cvt(&load_expr, target);
                    Ok(format!(
                        "    {} {} = {};\n",
                        type_name, packed_field_name, cvt_exp,
                    ))
                }
                GpuType::Vector(scalar, size) => {
                    let size_in_uints = size_in_uints(scalar.size() * size);
                    let load_expr = target.load_expr(current_offset, size_in_uints);
                    let cvt_exp = scalar.cvt_vec(&load_expr, *size, target);
                    Ok(format!(
                        "    {}{} {} = {};\n",
                        scalar.typename(target),
                        size,
                        packed_field_name,
                        cvt_exp,
                    ))
                }
                GpuType::InlineStruct(isn) => Ok(format!(
                    "    {}Packed {} = {}_read(buf, {});\n",
                    isn,
                    packed_field_name,
                    isn,
                    simplified_add("ref", current_offset)
                )),
                GpuType::Ref(inner) => {
                    if let GpuType::InlineStruct(isn) = inner.deref() {
                        Ok(format!(
                            "    {}Ref {} = {};\n",
                            isn,
                            packed_field_name,
                            target.load_expr(current_offset, 1),
                        ))
                    } else {
                        Ok(format!(
                            "    uint {} = {};\n",
                            packed_field_name,
                            target.load_expr(current_offset, 1),
                        ))
                    }
                }
            }
        } else {
            Err("cannot generate field reader from an open packed field".into())
        }
    }

    fn generate_field_writer(&self, offset: usize, target: TargetLang) -> String {
        let ty = self.ty.as_ref().unwrap();
        match ty {
            GpuType::Scalar(scalar) => {
                let value_exp = format!("s.{}", self.name);
                let cvt_exp = scalar.cvt_vec_inv(&value_exp, 1, target);
                format!("    {}\n", target.store_stmt(offset, 1, &cvt_exp))
            }
            GpuType::Vector(scalar, size) => {
                let size_in_uints = size_in_uints(scalar.size() * size);
                let value_exp = format!("s.{}", self.name);
                let cvt_exp = scalar.cvt_vec_inv(&value_exp, *size, target);
                format!(
                    "    {}\n",
                    target.store_stmt(offset, size_in_uints, &cvt_exp)
                )
            }
            GpuType::InlineStruct(isn) => format!(
                "    {}_write(buf, {}, s.{});\n",
                isn,
                simplified_add("ref", offset),
                self.name,
            ),
            GpuType::Ref(_) => {
                let value_exp = format!("s.{}", self.name);
                format!("    {}\n", target.store_stmt(offset, 1, &value_exp))
            }
        }
    }

    fn generate_accessor(
        &self,
        packed_struct_name: &str,
        ref_type: &str,
        reader: &str,
        target: TargetLang,
    ) -> Result<String, String> {
        if let Some(ty) = &self.ty {
            let mut field_accessor = String::new();

            match ty {
                GpuType::InlineStruct(name) => {
                    write!(
                        field_accessor,
                        "inline {}Packed {}_{}({}, {} ref) {{\n",
                        name,
                        packed_struct_name,
                        self.name,
                        target.buf_arg(),
                        ref_type,
                    )
                    .unwrap();
                }
                _ => {
                    write!(
                        field_accessor,
                        "inline {} {}_{}({}, {} ref) {{\n",
                        ty.unpacked_typename(target),
                        packed_struct_name,
                        self.name,
                        target.buf_arg(),
                        ref_type,
                    )
                    .unwrap();
                }
            }
            write!(field_accessor, "{}", reader).unwrap();
            write!(field_accessor, "    return {};\n}}\n\n", self.name).unwrap();

            Ok(field_accessor)
        } else {
            Err("cannot generate field accessor from open packed field".into())
        }
    }

    fn generate_unpackers(&self, packed_struct_name: &str, target: TargetLang) -> String {
        let mut unpackers = String::new();

        for sf in &self.stored_fields {
            write!(
                unpackers,
                "{}",
                sf.generate_unpacker(packed_struct_name, &self.name, target)
            )
            .unwrap();
        }

        unpackers
    }

    fn generate_packer(&self, target: TargetLang) -> String {
        format!("// packer for {}\n", self.name)
    }

    fn size(&self, module: &GpuModule) -> Result<usize, String> {
        if let Some(ty) = &self.ty {
            Ok(ty.size(module))
        } else {
            Err("cannot calculate size of open packed field".into())
        }
    }
}

impl PackedStruct {
    fn new(module: &GpuModule, name: &str, fields: &Vec<(String, GpuType)>) -> PackedStruct {
        let mut packed_fields: Vec<PackedField> = Vec::new();

        let mut current_packed_field = PackedField::new();
        for (field_name, ty) in fields {
            match current_packed_field.pack(module, &ty, &field_name).unwrap() {
                PackResult::SuccessAndClosed => {
                    packed_fields.push(current_packed_field);
                    current_packed_field = PackedField::new();
                }
                PackResult::FailAndClosed => {
                    packed_fields.push(current_packed_field);
                    current_packed_field = PackedField::new();
                    let res = current_packed_field.pack(module, &ty, &field_name).unwrap();
                    if res == PackResult::SuccessAndClosed {
                        packed_fields.push(current_packed_field);
                        current_packed_field = PackedField::new();
                    }
                }
                _ => {}
            }
        }

        if !current_packed_field.is_closed() {
            if !current_packed_field.is_empty() {
                current_packed_field.close(module).unwrap();
                packed_fields.push(current_packed_field);
            }
        }

        PackedStruct {
            name: format!("{}Packed", name),
            packed_fields,
            is_enum_variant: module.enum_variants.contains(name),
        }
    }

    fn generate_functions(&self, module: &GpuModule, target: TargetLang) -> String {
        let mut r = String::new();
        let mut field_accessors: Vec<String> = Vec::new();
        let mut unpackers: Vec<String> = Vec::new();

        // This is something of a hack to strip the "Packed" off the struct name
        let stripped_name = &self.name[0..self.name.len() - 6];
        let ref_type = format!("{}Ref", stripped_name);

        write!(
            r,
            "inline {} {}_read({}, {} ref) {{\n",
            self.name,
            stripped_name,
            target.buf_arg(),
            ref_type,
        )
        .unwrap();
        write!(r, "    {} result;\n\n", self.name).unwrap();

        let mut current_offset: usize = 0;
        if self.is_enum_variant {
            // account for tag
            current_offset = 4;
        }

        for packed_field in &self.packed_fields {
            let reader: String = packed_field
                .generate_reader(current_offset, target)
                .unwrap();
            let field_accessor: String = packed_field
                .generate_accessor(stripped_name, &ref_type, &reader, target)
                .unwrap();

            field_accessors.push(field_accessor);
            if packed_field.is_packed(false) {
                unpackers.push(packed_field.generate_unpackers(&self.name, target));
            }

            write!(r, "{}", reader).unwrap();
            write!(
                r,
                "    result.{} = {};\n\n",
                packed_field.name, packed_field.name
            )
            .unwrap();

            current_offset += packed_field.size(module).unwrap();
        }

        write!(r, "    return result;\n}}\n\n",).unwrap();

        for field_accessor in field_accessors {
            write!(r, "{}", field_accessor).unwrap();
        }

        for unpacker in unpackers {
            write!(r, "{}", unpacker).unwrap();
        }

        r
    }

    fn generate_structure_def(&self, target: TargetLang) -> String {
        let mut r = String::new();

        // The packed struct definition (is missing variable sized arrays)
        write!(r, "struct {} {{\n", self.name).unwrap();
        if self.is_enum_variant {
            write!(r, "    uint tag;\n").unwrap();
        }

        for packed_field in self.packed_fields.iter() {
            match packed_field.ty.as_ref().unwrap() {
                GpuType::InlineStruct(name) => {
                    // a packed struct will only store the packed version of any structs
                    write!(r, "    {}Packed {};\n", name, packed_field.name)
                }
                _ => write!(
                    r,
                    "    {} {};\n",
                    packed_field
                        .ty
                        .as_ref()
                        .expect(&format!("packed field {} has no type", packed_field.name))
                        .unpacked_typename(target),
                    packed_field.name
                ),
            }
            .unwrap()
        }
        write!(r, "{}", "};\n\n").unwrap();

        r
    }

    fn to_shader(&self, module: &GpuModule, target: TargetLang) -> String {
        let mut r = String::new();

        write!(r, "{}", self.generate_structure_def(target)).unwrap();
        write!(r, "{}", self.generate_functions(module, target)).unwrap();

        r
    }

    fn to_shader_wr(&self, module: &GpuModule, target: TargetLang) -> String {
        let mut r = String::new();

        write!(r, "// write functions for {}\n", self.name).unwrap();
        let mut packers: Vec<String> = Vec::new();

        // This is something of a hack to strip the "Packed" off the struct name
        let stripped_name = &self.name[0..self.name.len() - 6];
        let ref_type = format!("{}Ref", stripped_name);

        write!(
            r,
            "inline void {}_write({}, {} ref, {} s) {{\n",
            stripped_name,
            target.mut_buf_arg(),
            ref_type,
            self.name,
        )
        .unwrap();

        // The duplication here really suggests we have an intermediate representation
        // that has the offsets in place. But oh well.
        let mut current_offset: usize = 0;
        if self.is_enum_variant {
            // account for tag
            writeln!(
                r,
                "    {}",
                target.store_stmt(0, 1, "0 /* TODO: tag value */")
            )
            .unwrap();
            current_offset = 4;
        }

        for packed_field in &self.packed_fields {
            r.push_str(&packed_field.generate_field_writer(current_offset, target));

            current_offset += packed_field.size(module).unwrap();
        }

        write!(r, "}}\n\n",).unwrap();

        for packer in packers {
            write!(r, "{}", packer).unwrap();
        }

        r
    }
}

impl SpecifiedStruct {
    fn new(module: &GpuModule, name: &str, fields: Vec<(String, GpuType)>) -> SpecifiedStruct {
        let packed_form = PackedStruct::new(module, name, &fields);

        SpecifiedStruct {
            name: name.to_string(),
            fields,
            packed_form,
        }
    }

    fn generate_structure_def(&self, target: TargetLang) -> String {
        let mut r = String::new();

        // The unpacked struct definition (is missing variable sized arrays)
        write!(r, "struct {} {{\n", self.name).unwrap();

        for (field_name, field_type) in self.fields.iter() {
            write!(
                r,
                "    {} {};\n",
                field_type.unpacked_typename(target),
                field_name
            )
            .unwrap()
        }
        write!(r, "{}", "};\n\n").unwrap();

        r
    }

    fn generate_unpacker(&self) -> String {
        let mut r = String::new();

        write!(
            r,
            "inline {} {}_unpack({} packed_form) {{\n",
            self.name, self.name, self.packed_form.name,
        )
        .unwrap();

        write!(r, "    {} result;\n\n", self.name).unwrap();
        for (field_name, field_type) in self.fields.iter() {
            let packed_field = self
                .packed_form
                .packed_fields
                .iter()
                .find(|&pf| {
                    pf.stored_fields
                        .iter()
                        .find(|&sf| sf.name == field_name.as_str())
                        .is_some()
                })
                .expect(&format!(
                    "no packed field stores {} in {}Packed",
                    field_name, self.name
                ));
            if packed_field.is_packed(true) {
                match field_type {
                    GpuType::InlineStruct(name) => {
                        write!(
                            r,
                            "    result.{} = {}_unpack(packed_form.{});\n",
                            field_name, name, packed_field.name
                        )
                        .unwrap();
                    }
                    _ => {
                        write!(
                            r,
                            "    result.{} = {}_unpack_{}(packed_form.{});\n",
                            field_name, self.name, field_name, packed_field.name
                        )
                        .unwrap();
                    }
                }
            } else {
                write!(
                    r,
                    "    result.{} = packed_form.{};\n",
                    field_name, packed_field.name
                )
                .unwrap();
            }
        }
        write!(r, "{}", "\n    return result;\n}\n\n").unwrap();
        r
    }

    fn to_shader(&self, target: TargetLang) -> String {
        let mut r = String::new();

        write!(r, "{}", self.generate_structure_def(target)).unwrap();
        write!(r, "{}", self.generate_unpacker()).unwrap();

        r
    }
}

impl GpuType {
    // The type name for the *unpacked* version of the type.
    fn unpacked_typename(&self, target: TargetLang) -> String {
        match self {
            GpuType::Scalar(scalar) => scalar.unpacked_type(target).typename(target).into(),
            GpuType::Vector(scalar, size) => match scalar {
                GpuScalar::F32 | GpuScalar::I32 | GpuScalar::U32 => {
                    format!("{}{}", scalar.unpacked_type(target).typename(target), size)
                }
                _ => {
                    if *size == 1 {
                        "uint".into()
                    } else {
                        format!("uint{}", size)
                    }
                }
            },
            GpuType::InlineStruct(name) => name.to_string(),
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
        match self {
            GpuType::InlineStruct(_) => 4,
            _ => {
                let size = self.size(module);
                if size >= 4 {
                    4
                } else {
                    1
                }
            }
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

    /// Generate a Rust type.
    fn gen_derive(&self, module: &GpuModule) -> proc_macro2::TokenStream {
        match self {
            GpuType::Scalar(s) => s.gen_derive(),
            GpuType::Vector(s, len) => {
                let scalar = s.gen_derive();
                quote! { [#scalar; #len] }
            }
            GpuType::InlineStruct(name) => {
                let name_id = format_ident!("{}", name);
                quote! { #name_id }
            }
            GpuType::Ref(ty) => {
                let gen_ty = ty.gen_derive(module);
                quote! { crate::encoder::Ref<#gen_ty> }
            }
        }
    }

    fn gen_encode_field(&self, offset: usize, name: &str) -> proc_macro2::TokenStream {
        let name_id = format_ident!("{}", name);
        match self {
            GpuType::Scalar(s) => {
                let end = offset + s.size();
                quote! {
                    buf[#offset..#end].copy_from_slice(&self.#name_id.to_le_bytes());
                }
            }
            GpuType::Vector(s, len) => {
                let size = s.size();
                quote! {
                    for i in 0..#len {
                        let offset = #offset + i * #size;
                        buf[offset..offset + #size].copy_from_slice(&self.#name_id[i].to_le_bytes());
                    }
                }
            }
            GpuType::Ref(_) => {
                quote! {
                    buf[#offset..#offset + 4].copy_from_slice(&self.#name_id.offset().to_le_bytes());
                }
            }
            _ => {
                quote! {
                    &self.#name_id.encode_to(&mut buf[#offset..]);
                }
            }
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
                        offset += align_padding(offset, field.alignment(module));
                        offset += field.size(module);
                    }
                    max_offset = max_offset.max(offset);
                }
                max_offset
            }
        }
    }

    /// Alignment of the body of the definition.
    ///
    /// TODO: all structures are aligned to 4 bytes, not useful :/
    fn alignment(&self, module: &GpuModule) -> usize {
        match self {
            GpuTypeDef::Struct(name, fields) => 4,
            GpuTypeDef::Enum(_en) => unimplemented!(),
        }
    }

    // TODO: implement this in new scheme
    /*
    fn to_metal_load_enum(&self, enum_name: &str, module: &GpuModule, r: &mut String) {
        match self {
            GpuTypeDef::Struct(name, fields) => {
                write!(
                    r,
                    "{}Packed {}_load(const thread {} &s) {{\n",
                    name, name, enum_name
                )
                .unwrap();
                write!(r, "    {}Packed r;\n", name).unwrap();
                write!(r, "    r.tag = s.tag;\n").unwrap();
                let mut offset = 4;
                for (fieldname, ty) in fields {
                    offset += align_padding(offset, ty.alignment(module));
                    let mty = ty.metal_typename();
                    // maybe better to load from `body` array rather than pointer foo
                    write!(
                        r,
                        "    r.{} = *((const thread {} *)((const thread char *)&s + {}));\n",
                        fieldname, mty, offset
                    )
                    .unwrap();
                    offset += ty.size(module);
                }
                write!(r, "    return r;\n").unwrap();
                write!(r, "}}\n").unwrap();
            }
            _ => panic!("internal inconsistency"),
        }
    }
    */

    // TODO: implement writers
    /*
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
                write!(r, "    *((device {}Packed *)(buf + ref)) = s;\n", name).unwrap();
                write!(r, "}}\n").unwrap();
            }
            // We don't write individual enum structs, we only write their variants.
            GpuTypeDef::Enum(en) => {
                if en.variants.iter().any(|(_name, fields)| fields.is_empty()) {
                    write!(
                        r,
                        "void {}_write_tag(device char *buf, CmdRef ref, uint tag) {{\n",
                        en.name
                    )
                    .unwrap();
                    write!(r, "    ((device {} *)(buf + ref))->tag = tag;\n", en.name).unwrap();
                    write!(r, "}}\n").unwrap();
                }
            }
        }
        r
    }
    */

    fn to_shader(&self, module: &GpuModule, target: TargetLang) -> String {
        let mut r = String::new();

        match self {
            GpuTypeDef::Struct(name, fields) => {
                let structure = SpecifiedStruct::new(module, name, fields.clone());
                write!(r, "{}", structure.packed_form.to_shader(module, target)).unwrap();
                write!(r, "{}", structure.to_shader(target)).unwrap();
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
                    "inline uint {}_tag({}, {} ref) {{\n",
                    en.name,
                    target.buf_arg(),
                    rn
                )
                .unwrap();

                write!(
                    r,
                    "    uint result = {};\n    return result;\n",
                    target.load_expr(0, 1)
                )
                .unwrap();
                write!(r, "}}\n\n").unwrap();

                if target == TargetLang::Hlsl {
                    let quotient_in_u32x4 = size / (4 * GpuScalar::U32.size());
                    let remainder_in_u32s = (size / 4) % 4;
                    write!(r, "inline void {}_copy(ByteAddressBuffer src, uint src_ref, RWByteAddressBuffer dst, uint dst_ref) {{\n", en.name).unwrap();
                    for i in 0..quotient_in_u32x4 {
                        write!(
                            r,
                            "    uint4 group{} = src.Load4({});\n",
                            i,
                            simplified_add("src_ref", i * 4 * 4)
                        )
                        .unwrap();
                        write!(
                            r,
                            "    dst.Store4({}, group{});\n",
                            simplified_add("dst_ref", i * 4 * 4),
                            i,
                        )
                        .unwrap();
                    }
                    if remainder_in_u32s > 0 {
                        let tail = vector_size_str(remainder_in_u32s);
                        write!(
                            r,
                            "\n    uint{} group{} = src.Load{}({});\n",
                            tail,
                            quotient_in_u32x4,
                            tail,
                            simplified_add("src_ref", quotient_in_u32x4 * 4 * 4)
                        )
                        .unwrap();
                        write!(
                            r,
                            "    dst.Store{}({}, group{});\n",
                            tail,
                            simplified_add("dst_ref", quotient_in_u32x4 * 4 * 4),
                            quotient_in_u32x4
                        )
                        .unwrap();
                    }
                    write!(r, "{}", "}\n\n").unwrap();
                }
            }
        }
        r
    }

    fn to_shader_wr(&self, module: &GpuModule, target: TargetLang) -> String {
        let mut r = String::new();
        match self {
            GpuTypeDef::Struct(name, fields) => {
                // Write of packed structure
                writeln!(r, "// TODO: writer for struct {}", name).unwrap();
                let structure = SpecifiedStruct::new(module, name, fields.clone());
                write!(r, "{}", structure.packed_form.to_shader_wr(module, target)).unwrap();
                /*
                write!(
                    r,
                    "void {}_write(device char *buf, {}Ref ref, {}Packed s) {{\n",
                    name, name, name
                )
                .unwrap();
                write!(r, "    *((device {}Packed *)(buf + ref)) = s;\n", name).unwrap();
                write!(r, "}}\n").unwrap();
                */
            }
            // We don't write individual enum structs, we only write their variants.
            GpuTypeDef::Enum(en) => {
                if en.variants.iter().any(|(_name, fields)| fields.is_empty()) {
                    writeln!(
                        r,
                        "void {}_write_tag({}, CmdRef ref, uint tag) {{",
                        en.name,
                        target.mut_buf_arg(),
                    )
                    .unwrap();
                    writeln!(r, "    {}", target.store_stmt(0, 1, "tag")).unwrap();
                    writeln!(r, "}}").unwrap();
                }
            }
        }
        r
    }

    /// Generate a struct/enum and encoder impl for the type.
    fn gen_derive(&self, module: &GpuModule) -> proc_macro2::TokenStream {
        match self {
            GpuTypeDef::Struct(name, fields) => {
                let name_id = format_ident!("{}", name);
                let mut gen_fields = proc_macro2::TokenStream::new();
                for (field_name, ty) in fields {
                    let field_name_id = format_ident!("{}", field_name);
                    let gen_ty = ty.gen_derive(module);
                    let gen_field = quote! {
                        pub #field_name_id: #gen_ty,
                    };
                    gen_fields.extend(gen_field);
                }
                let encoded_size = self.size(module);
                // A note on offsets. The logic for computing the offset is duplicated with
                // the logic for packing in the shader code; it's important that these sync
                // up. It would be better to have one source of truth.
                let mut offset = 0;
                if module.enum_variants.contains(name) {
                    offset += 4;
                }

                let mut encode_fields = proc_macro2::TokenStream::new();
                for (field_name, ty) in fields {
                    offset += align_padding(offset, ty.alignment(module));
                    let encode_field = ty.gen_encode_field(offset, field_name);
                    offset += ty.size(module);
                    encode_fields.extend(encode_field);
                }

                quote! {
                    pub struct #name_id {
                        #gen_fields
                    }

                    impl crate::encoder::Encode for #name_id {
                        fn fixed_size() -> usize {
                            #encoded_size
                        }
                        fn encode_to(&self, buf: &mut [u8]) {
                            #encode_fields
                        }
                    }
                }
            }
            GpuTypeDef::Enum(en) => {
                let enum_name = format_ident!("{}", en.name);
                let mut variants = proc_macro2::TokenStream::new();
                let mut cases = proc_macro2::TokenStream::new();
                let mut variant_ix = 0u32;
                for (variant_name, fields) in &en.variants {
                    let variant_id = format_ident!("{}", variant_name);
                    let field_tys = fields.iter().map(|field| field.gen_derive(module));
                    let variant = quote! {
                        #variant_id(#(#field_tys),*),
                    };
                    variants.extend(variant);
                    let mut offset = 4;
                    let mut args = Vec::new();
                    let mut field_encoders = proc_macro2::TokenStream::new();
                    for (ix, field) in fields.iter().enumerate() {
                        let field_id = format_ident!("f{}", ix);
                        if let GpuType::InlineStruct(_) = field {
                            if offset == 4 {
                                offset = 0;
                            }
                        }
                        offset += align_padding(offset, field.alignment(module));
                        let field_encoder = quote! {
                            #field_id.encode_to(&mut buf[#offset..]);
                        };
                        field_encoders.extend(field_encoder);
                        args.push(field_id);
                        offset += field.size(module);
                    }
                    let case = quote! {
                        #enum_name::#variant_id(#(#args),*) => {
                            buf[0..4].copy_from_slice(&#variant_ix.to_le_bytes());
                            // TODO: set offset for field
                            #field_encoders
                        }
                    };
                    cases.extend(case);
                    variant_ix += 1;
                }
                let encoded_size = self.size(module);
                quote! {
                    pub enum #enum_name {
                        #variants
                    }

                    impl crate::encoder::Encode for #enum_name {
                        fn fixed_size() -> usize {
                            #encoded_size
                        }
                        fn encode_to(&self, buf: &mut [u8]) {
                            match self {
                                #cases
                            }
                        }
                    }
                }
            }
        }
    }
}

impl GpuModule {
    fn from_syn(module: &syn::ItemMod) -> Result<Self, String> {
        let name = module.ident.to_string();
        let mut attrs = HashSet::new();
        for attr in &module.attrs {
            if let Some(id) = path_as_single_ident(&attr.path) {
                attrs.insert(id.to_owned());
            }
        }
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
            attrs,
            enum_variants,
            defs,
        })
    }

    fn resolve_by_name(&self, name: &str) -> Result<&GpuTypeDef, String> {
        for def in &self.defs {
            if def.name() == name {
                return Ok(&def);
            }
        }
        Err(format!("could not find {} in module", name))
    }

    fn to_shader(&self, target: TargetLang) -> String {
        let mut r = String::new();

        write!(&mut r, "{}", generate_value_extractor(8)).unwrap();
        write!(&mut r, "{}", generate_value_extractor(16)).unwrap();

        for def in &self.defs {
            match def {
                GpuTypeDef::Struct(name, _) => {
                    write!(&mut r, "typedef uint {}Ref;\n", name).unwrap();
                }
                GpuTypeDef::Enum(_) => {
                    write!(&mut r, "typedef uint {}Ref;\n", def.name()).unwrap();
                }
            }
        }

        write!(&mut r, "\n").unwrap();
        for def in &self.defs {
            r.push_str(&def.to_shader(self, target));
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

        if self.attrs.contains("gpu_write") {
            for def in &self.defs {
                r.push_str(&def.to_shader_wr(self, target));
            }
        }
        r
    }

    fn gen_derive(&self) -> proc_macro2::TokenStream {
        let mut ts = proc_macro2::TokenStream::new();
        let module_name = format_ident!("{}", self.name);
        for def in &self.defs {
            let def_ts = def.gen_derive(self);
            ts.extend(def_ts);
        }
        quote! {
            mod #module_name {
                #ts
            }
        }
    }
}

fn path_as_single_ident(path: &syn::Path) -> Option<String> {
    if path.segments.len() == 1 {
        let seg = &path.segments[0];
        if seg.arguments == PathArguments::None {
            return Some(seg.ident.to_string());
        }
    }
    None
}

fn ty_as_single_ident(ty: &syn::Type) -> Option<String> {
    if let syn::Type::Path(TypePath { path, .. }) = ty {
        path_as_single_ident(path)
    } else {
        None
    }
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
pub fn piet_gpu(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::ItemMod);
    //println!("input: {:#?}", input);
    let module = GpuModule::from_syn(&input).unwrap();
    let gen_gpu_fn = format_ident!("gen_gpu_{}", input.ident);
    let hlsl_result = module.to_shader(TargetLang::Hlsl);
    let msl_result = module.to_shader(TargetLang::Msl);
    let mut expanded = quote! {
        pub fn #gen_gpu_fn(lang: &str) -> String {
            match lang {
                "HLSL" => #hlsl_result.into(),
                "MSL" => #msl_result.into(),
                _ => panic!("unknown shader lang {}", lang),
            }
        }
    };
    if module.attrs.contains("rust_encode") {
        let foo = module.gen_derive();
        expanded.extend(foo);
    }
    expanded.into()
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
