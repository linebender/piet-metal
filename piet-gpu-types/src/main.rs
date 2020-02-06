#[macro_use]
extern crate piet_gpu_derive;

use piet_gpu_types::encoder::{Encode, Encoder};

piet_gpu! {
    #[rust_encode]
    mod scene {
        struct SimpleGroup {
            n_items: u32,
            // This should actually be a variable size array.
            items_ix: Ref<PietItem>,
            // Note: we want a variable size array of bboxes
            bbox: [u16; 4],
        }
        struct PietCircle {
        }
        struct PietStrokeLine {
            flags: u32,
            rgba_color: u32,
            width: f32,
            start: [f32; 2],
            end: [f32; 2],
        }
        struct PietFill {
            flags: u32,
            rgba_color: u32,
            n_points: u32,
            points_ix: Ref<f32>,
        }
        struct PietStrokePolyLine {
            rgba_color: u32,
            width: f32,
            n_points: u32,
            points_ix: Ref<f32>,
        }
        enum PietItem {
            Circle(PietCircle),
            Line(PietStrokeLine),
            Fill(PietFill),
            Poly(PietStrokePolyLine),
        }
    }
}

fn main() {
}
