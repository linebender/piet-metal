#[macro_use]
extern crate piet_metal_derive;

//#[derive(PietMetal)]
struct SimpleGroup {
    n_items: u32,
    items_ix: u32,
    // TODO: bbox
}

piet_metal! {
    mod scene {
        struct A {}
        struct B {}
        struct Bbox {
            bbox: [u32; 4],
        }
        struct SimpleGroup {
            n_items: u32,
            items_ix: u32,
            // TODO: bbox
        }
    }
}

fn main() {
    //foo();
    gen_metal_scene();
}
