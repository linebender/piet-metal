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
use syn::{parse::Parse, parse::ParseStream, parse_macro_input};
use syn::{
    Data, Expr, ExprLit, Fields, FieldsNamed, FieldsUnnamed, GenericArgument, ItemEnum, ItemStruct,
    Lit, PathArguments, TypeArray, TypePath,
};

/// The target shader language. We can't make this a public type because of Rust rules.
#[derive(Copy, Clone, PartialEq)]
enum TargetLang {
    Hlsl,
    Msl,
}

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
    #[allow(unused)]
    name: String,
    /// Set of item names that are used as enum variants.
    enum_variants: HashSet<String>,
    defs: Vec<GpuTypeDef>,
}

impl TargetLang {
    /// The typed function argument for "buf"
    fn buf_arg(self) -> &'static str {
        match self {
            TargetLang::Hlsl => "ByteAddressBuffer buf",
            TargetLang::Msl => "const device *buf",
        }
    }

    /// An expression for loading a number of uints.
    fn load_expr(self, offset: usize, size: usize) -> String {
        let tail = if offset == 0 {
            "".into()
        } else {
            format!(" + {}", offset)
        };
        let size_str = match size {
            1 => "",
            2 => "2",
            3 => "3",
            4 => "4",
            _ => panic!("invalid vector size {}", size),
        };
        match self {
            TargetLang::Hlsl => format!("buf.Load{}(ref{})", size_str, tail),
            TargetLang::Msl => {
                let packed = if size == 1 { "" } else { "packed_" };
                format!(
                    "*(device const *{}uint{})(buf + ref{})",
                    packed, size_str, tail
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

    // deprecated
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

    // deprecated
    fn hlsl_typename(self) -> &'static str {
        match self {
            GpuScalar::F32 => "float",
            GpuScalar::I8 | GpuScalar::I16 | GpuScalar::I32 => "int",
            GpuScalar::U8 | GpuScalar::U16 | GpuScalar::U32 => "uint",
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
        match (target, self) {
            (TargetLang::Hlsl, GpuScalar::F32) => format!("asfloat({})", inner),
            (TargetLang::Hlsl, GpuScalar::I32) => format!("asint({})", inner),
            (TargetLang::Msl, GpuScalar::F32) => format!("as_type<float>({})", inner),
            (TargetLang::Msl, GpuScalar::I32) => format!("as_type<int>({})", inner),
            (TargetLang::Msl, GpuScalar::I16) => format!("as_type<short>({})", inner),
            (TargetLang::Msl, GpuScalar::I8) => format!("as_type<char>({})", inner),
            (TargetLang::Msl, GpuScalar::U16) => format!("as_type<ushort>({})", inner),
            (TargetLang::Msl, GpuScalar::U8) => format!("as_type<uchar>({})", inner),
            // TODO: probably need to be smarter about signed int conversion
            _ => inner.into(),
        }
    }

    /// Convert a uint vector into the given vector
    fn cvt_vec(self, inner: &str, size: usize, target: TargetLang) -> String {
        match (target, self) {
            (TargetLang::Hlsl, GpuScalar::F32) => format!("asfloat({})", inner),
            (TargetLang::Hlsl, GpuScalar::I32) => format!("asint({})", inner),
            (TargetLang::Msl, GpuScalar::F32) => format!("as_type<float{}>({})", size, inner),
            (TargetLang::Msl, GpuScalar::I32) => format!("as_type<int{}>({})", size, inner),
            (TargetLang::Msl, GpuScalar::I16) => format!("as_type<short{}>({})", size, inner),
            (TargetLang::Msl, GpuScalar::I8) => format!("as_type<char{}>({})", size, inner),
            (TargetLang::Msl, GpuScalar::U16) => format!("as_type<ushort{}>({})", size, inner),
            (TargetLang::Msl, GpuScalar::U8) => format!("as_type<uchar{}>({})", size, inner),
            // TODO: probably need to be smarter about signed int conversion
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

/// Return number of `uints` required to store `num_bytes` bytes.
fn size_in_uints(num_bytes: usize) -> usize {
    // a `uint` has a size of 4 bytes, (size_in_bytes + 4 - 1) / 4
    (num_bytes + 3) / 4
}

// TODO: this only generates unsigned extractors, we will need signed as well.
fn generate_hlsl_value_extractor(size_in_bits: u32) -> String {
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
    fn generate_hlsl_unpacker(
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
                        _ => String::from(scalar.hlsl_typename()),
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
                    let hlsl_typename = self.ty.unpacked_typename(target);

                    let size_in_uints = size_in_uints(&scalar.size() * unpacked_size);
                    write!(
                        unpacker,
                        "inline {} {}_unpack_{}(uint{} {}) {{\n    {} result;\n\n",
                        hlsl_typename,
                        stripped_name,
                        self.name,
                        size_in_uints,
                        packed_field_name,
                        hlsl_typename,
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
                    self.ty.hlsl_typename()
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
    fn is_packed(&self) -> bool {
        if self.stored_fields.len() != 1 {
            return true;
        }
        match self.stored_fields[0].ty {
            GpuType::Scalar(scalar) => scalar.size() < 4,
            GpuType::Vector(scalar, _) => scalar.size() < 4,
            // Maybe we should be sophisticated about inline structs
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

                if self.is_packed() {
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

    fn generate_hlsl_reader(
        &self,
        current_offset: usize,
        target: TargetLang,
    ) -> Result<String, String> {
        if let Some(ty) = &self.ty {
            let type_name = ty.hlsl_typename();
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
                        scalar.hlsl_typename(),
                        size,
                        packed_field_name,
                        cvt_exp,
                    ))
                }
                GpuType::InlineStruct(isn) => Ok(format!(
                    "    {}Packed {} = {}Packed_read(buf, {});\n",
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

    fn generate_hlsl_accessor(
        &self,
        packed_struct_name: &str,
        ref_type: &str,
        reader: &str,
        target: TargetLang,
    ) -> Result<String, String> {
        if let Some(ty) = &self.ty {
            let mut field_accessor = String::new();

            match ty {
                GpuType::InlineStruct(_) => {
                    write!(
                        field_accessor,
                        "inline {}Packed {}_{}({}, {} ref) {{\n",
                        ty.hlsl_typename(),
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
                        ty.hlsl_typename(),
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

    fn generate_hlsl_unpackers(&self, packed_struct_name: &str, target: TargetLang) -> String {
        let mut unpackers = String::new();

        for sf in &self.stored_fields {
            write!(
                unpackers,
                "{}",
                sf.generate_hlsl_unpacker(packed_struct_name, &self.name, target)
            )
            .unwrap();
        }

        unpackers
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

    fn generate_hlsl_functions(&self, module: &GpuModule, target: TargetLang) -> String {
        let mut r = String::new();
        let mut field_accessors: Vec<String> = Vec::new();
        let mut unpackers: Vec<String> = Vec::new();

        // This is something of a hack to strip the "Packed" off the struct name
        let stripped_name = &self.name[0..self.name.len() - 6];
        let ref_type = format!("{}Ref", stripped_name);

        write!(
            r,
            "inline {} {}_read({}, {} ref) {{\n",
            stripped_name,
            self.name,
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
                .generate_hlsl_reader(current_offset, target)
                .unwrap();
            let field_accessor: String = packed_field
                .generate_hlsl_accessor(stripped_name, &ref_type, &reader, target)
                .unwrap();

            field_accessors.push(field_accessor);
            if packed_field.is_packed() {
                unpackers.push(packed_field.generate_hlsl_unpackers(&self.name, target));
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

    fn generate_hlsl_structure_def(&self) -> String {
        let mut r = String::new();

        // The unpacked struct definition (is missing variable sized arrays)
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
                        .hlsl_typename(),
                    packed_field.name
                ),
            }
            .unwrap()
        }
        write!(r, "{}", "};\n\n").unwrap();

        r
    }

    fn to_hlsl(&self, module: &GpuModule, target: TargetLang) -> String {
        let mut r = String::new();

        write!(r, "{}", self.generate_hlsl_structure_def()).unwrap();
        write!(r, "{}", self.generate_hlsl_functions(module, target)).unwrap();

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

    fn generate_hlsl_structure_def(&self, target: TargetLang) -> String {
        let mut r = String::new();

        // The packed struct definition (is missing variable sized arrays)
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

    fn generate_hlsl_unpacker(&self) -> String {
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
            if packed_field.is_packed() {
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

    fn to_hlsl(&self, target: TargetLang) -> String {
        let mut r = String::new();

        write!(r, "{}", self.generate_hlsl_structure_def(target)).unwrap();
        write!(r, "{}", self.generate_hlsl_unpacker()).unwrap();

        r
    }
}

impl GpuType {
    /// The shader language name for the unpacked version of the type.
    fn unpacked_typename(&self, target: TargetLang) -> String {
        match self {
            GpuType::Scalar(scalar) => scalar.unpacked_type(target).typename(target).into(),
            GpuType::Vector(scalar, size) => {
                format!("{}{}", scalar.unpacked_type(target).typename(target), size)
            }
            GpuType::InlineStruct(name) => format!("{}", name),
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

    // The type name for the *unpacked* version of the type.
    fn hlsl_typename(&self) -> String {
        match self {
            GpuType::Scalar(scalar) => scalar.hlsl_typename().into(),
            GpuType::Vector(scalar, size) => match scalar {
                GpuScalar::F32 | GpuScalar::I32 | GpuScalar::U32 => {
                    format!("{}{}", scalar.hlsl_typename(), size)
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
                for (field_name, ty) in fields {
                    write!(r, "    {} {};\n", ty.metal_typename(), field_name).unwrap();
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
                for (field_name, ty) in fields {
                    if ty.is_small() {
                        let tn = ty.metal_typename();
                        write!(
                            r,
                            "{} {}_{}(const device char *buf, {} ref) {{\n",
                            tn, name, field_name, rn
                        )
                        .unwrap();
                        write!(
                            r,
                            "    return ((const device {}Packed *)(buf + ref))->{};\n",
                            name, field_name
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
                // Read of entire structure
                write!(
                    r,
                    "{} {}_read(const device char *buf, {} ref) {{\n",
                    en.name, en.name, rn
                )
                .unwrap();
                write!(
                    r,
                    "    return *((const device {} *)(buf + ref));\n",
                    en.name
                )
                .unwrap();
                write!(r, "}}\n").unwrap();
                // Read of just the tag
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
                for (name, fields) in &en.variants {
                    write!(r, "#define {}_{} {}\n", en.name, name, tag).unwrap();
                    tag += 1;

                    if !fields.is_empty() {
                        if let GpuType::InlineStruct(s) = &fields[0] {
                            let s = module.resolve_by_name(&s).unwrap();
                            s.to_metal_load_enum(&en.name, module, &mut r);
                        }
                    }
                }
            }
        }
        r
    }

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

    fn to_hlsl(&self, module: &GpuModule, target: TargetLang) -> String {
        let mut r = String::new();

        match self {
            GpuTypeDef::Struct(name, fields) => {
                let structure = SpecifiedStruct::new(module, name, fields.clone());
                write!(r, "{}", structure.packed_form.to_hlsl(module, target)).unwrap();
                write!(r, "{}", structure.to_hlsl(target)).unwrap();
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
                    write!(r, "inline void {}_read_into(ByteAddressBuffer src, uint src_ref, RWByteAddressBuffer dst, uint dst_ref) {{\n", en.name).unwrap();
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
                        let tail = match remainder_in_u32s {
                            2 => "2",
                            3 => "3",
                            _ => "",
                        };
                        write!(
                            r,
                            "\n    uint{} group{} = src.Load{}({});\n",
                            tail,
                            quotient_in_u32x4,
                            tail,
                            simplified_add("src_ref", quotient_in_u32x4 * 4)
                        )
                        .unwrap();
                        write!(
                            r,
                            "    dst.Store{}({});\n",
                            tail,
                            simplified_add("dst_ref", quotient_in_u32x4 * 4)
                        )
                        .unwrap();
                    }
                    write!(r, "{}", "}\n\n").unwrap();
                }
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

    fn resolve_by_name(&self, name: &str) -> Result<&GpuTypeDef, String> {
        for def in &self.defs {
            if def.name() == name {
                return Ok(&def);
            }
        }
        Err(format!("could not find {} in module", name))
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

    fn to_hlsl(&self, target: TargetLang) -> String {
        let mut r = String::new();

        write!(&mut r, "{}", generate_hlsl_value_extractor(8)).unwrap();
        write!(&mut r, "{}", generate_hlsl_value_extractor(16)).unwrap();

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
            r.push_str(&def.to_hlsl(self, target));
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

#[proc_macro]
pub fn piet_hlsl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::ItemMod);
    //println!("input: {:#?}", input);
    let module = GpuModule::from_syn(&input).unwrap();
    let gen_hlsl_fn = format_ident!("gen_hlsl_{}", input.ident);
    let result = module.to_hlsl(TargetLang::Hlsl);
    let expanded = quote! {
        fn #gen_hlsl_fn() -> String {
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
