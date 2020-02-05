//  Copyright 2020 The xi-editor authors.

//! New-style encoders (supporting proc macros)

pub struct A;

/// A reference to an encoded object within a buffer
#[derive(Clone, Copy, Debug)]
pub struct Ref<T> {
    offset: u32,
    _phantom: std::marker::PhantomData<T>,
}

pub struct Encoder {
    buf: Vec<u8>,
}

// TODO: we probably do want to encode slices, get rid of Sized bound
pub trait Encode: Sized {
    fn encoded_size(&self) -> usize;

    fn encode_to(&self, buf: &mut [u8]);

    fn encode(&self, encoder: &mut Encoder) -> Ref<Self> {
        let size = self.encoded_size();
        let (offset, buf) = encoder.alloc_chunk(size as u32);
        self.encode_to(buf);
        Ref::new(offset)
    }
}

impl<T> Ref<T> {
    fn new(offset: u32) -> Ref<T> {
        Ref {
            offset,
            _phantom: Default::default(),
        }
    }
}
impl Encoder {
    pub fn new() -> Encoder {
        Encoder {
            buf: Vec::new(),
        }
    }

    pub fn alloc_chunk(&mut self, size: u32) -> (u32, &mut [u8]) {
        let offset = self.buf.len();
        self.buf.resize(size as usize + offset, 0);
        (offset as u32, &mut self.buf[offset..])
    }

    pub fn buf(&self) -> &[u8] {
        &self.buf
    }
}

impl<T> Encode for Ref<T> {
    fn encoded_size(&self) -> usize {
        4
    }

    fn encode_to(&self, buf: &mut [u8]) {
        buf[0..4].copy_from_slice(&self.offset.to_le_bytes());
    }
}

impl Encode for u32 {
    fn encoded_size(&self) -> usize {
        4
    }

    fn encode_to(&self, buf: &mut [u8]) {
        buf[0..4].copy_from_slice(&self.to_le_bytes());
    }
}

impl Encode for f32 {
    fn encoded_size(&self) -> usize {
        4
    }

    fn encode_to(&self, buf: &mut [u8]) {
        buf[0..4].copy_from_slice(&self.to_le_bytes());
    }
}
