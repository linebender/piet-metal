#[macro_use]
extern crate piet_gpu_derive;

//#[derive(PietMetal)]
struct SimpleGroup {
    n_items: u32,
    items_ix: u32,
    // TODO: bbox
}

piet_gpu! {
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

piet_gpu! {
    mod ptcl {
        struct CmdCircle {
            // In existing code, this is packed; we might need an annotation for this.
            bbox: [u16; 4],
        }
        struct CmdLine {
            start: [f32; 2],
            end: [f32; 2],
        }
        struct CmdStroke {
            // In existing code, this is f16. Should we have support?
            halfWidth: f32,
            rgba_color: u32,
        }
        struct CmdFill {
            start: [f32; 2],
            end: [f32; 2],
        }
        struct CmdFillEdge {
            // The sign is only one bit.
            sign: i32,
            y: f32,
        }
        struct CmdDrawFill {
            backdrop: i32,
            rgba_color: u32,
        }
        struct CmdSolid {
            rgba_color: u32,
        }
        enum Cmd {
            End,
            Circle(CmdCircle),
            Line(CmdLine),
            Fill(CmdFill),
            Stroke(CmdStroke),
            FillEdge(CmdFillEdge),
            DrawFill(CmdDrawFill),
            Solid(CmdSolid),
            Bail,
        }
    }
}

fn main() {
    print!("{}", gen_gpu_scene("MSL"));
}
