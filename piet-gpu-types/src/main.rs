#[macro_use]
extern crate piet_gpu_derive;

use piet_gpu_types::encoder::{Encode, Encoder};

piet_gpu! {
    #[rust_encode]
    mod test {
        struct B {
            b: u32,
            c: f32,
            d: [u8; 4],
        }
        enum E {
            E0(u32),
            E1(B),
            E2(Ref<B>),
        }
    }
}

fn main() {
    let mut encoder = Encoder::new();
    let b = test::B { b: 1, c: 12.5, d: [2, 3, 5, 8] };
    let r2 = b.encode(&mut encoder);
    println!("buf: {:?}", encoder.buf());
}
