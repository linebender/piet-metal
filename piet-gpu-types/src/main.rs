#[macro_use]
extern crate piet_gpu_derive;

use piet_gpu_types::encoder::{Encode, Encoder};

fn main() {
    print!("{}", piet_gpu_types::scene::gen_gpu_scene("MSL"));
    print!("{}", piet_gpu_types::ptcl::gen_gpu_ptcl("MSL"));
}
