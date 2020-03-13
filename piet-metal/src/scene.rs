//! Low level mechanisms for encoding the scene graph.

use kurbo::{BezPath, Circle, Line, Point, Rect, Shape};

use piet_gpu_types::encoder::{Encode, Encoder, Ref};
use piet_gpu_types::scene::{
    PietCircle, PietFill, PietItem, PietStrokeLine, PietStrokePolyLine, SimpleGroup,
};

use crate::flatten::flatten_path;

pub struct SceneEncoder {
    encoder: Encoder,
}

#[derive(Default)]
pub struct GroupEncoder {
    bboxes: Vec<[u16; 4]>,
    items: Vec<PietItem>,
}

// This is a tradeoff between smoothness and contrast, set by aesthetic preference. The
// optimum rendering of very thin strokes is likely an area for further research.
const THIN_LINE: f32 = 0.7;
const TOLERANCE: f64 = 0.1;

impl SceneEncoder {
    pub fn new() -> SceneEncoder {
        let mut encoder = Encoder::new();
        let reserve_root = encoder.alloc_chunk(SimpleGroup::fixed_size() as u32);
        debug_assert_eq!(reserve_root.0, 0);
        SceneEncoder { encoder }
    }

    pub fn encode_root_group(&mut self, group: GroupEncoder) {
        let bboxes = group.bboxes.encode(&mut self.encoder).transmute();
        let items = group.items.encode(&mut self.encoder).transmute();
        let simple_group = SimpleGroup {
            n_items: group.bboxes.len() as u32,
            bboxes,
            items,
        };
        simple_group.encode_to(&mut self.encoder.buf_mut()[0..SimpleGroup::fixed_size()]);
    }

    pub fn circle(&mut self, group: &mut GroupEncoder, circle: &Circle) {
        let piet_circle = PietCircle {};
        let item = PietItem::Circle(piet_circle);
        group.push_item(item, circle.bounding_box());
    }

    pub fn stroke_line(&mut self, group: &mut GroupEncoder, line: Line, width: f32, rgba: u32) {
        let piet_stroke_line = PietStrokeLine {
            flags: Default::default(),
            rgba_color: rgba.to_be(),
            width,
            start: point_to_f32s(line.p0),
            end: point_to_f32s(line.p1),
        };
        let item = PietItem::Line(piet_stroke_line);
        // TODO: do we need to add an additional 0.5?
        let hw = (width * 0.5) as f64;
        let bbox = line.bounding_box().inflate(hw, hw);
        group.push_item(item, bbox);
    }

    pub fn polyline(&mut self, group: &mut GroupEncoder, points: &[Point], width: f32, rgba: u32) {
        let (points_ix, bbox) = self.encode_points(points);
        let piet_poly = PietStrokePolyLine {
            rgba_color: rgba.to_be(),
            width,
            n_points: points.len() as u32,
            points_ix: points_ix,
        };
        let item = PietItem::Poly(piet_poly);
        let hw = (width * 0.5) as f64;
        let bbox = bbox.inflate(hw, hw);
        group.push_item(item, bbox);
    }

    pub fn fill_polyline(&mut self, group: &mut GroupEncoder, points: &[Point], rgba: u32) {
        let (points_ix, bbox) = self.encode_points(points);
        let piet_fill = PietFill {
            flags: 0,
            rgba_color: rgba.to_be(),
            n_points: points.len() as u32,
            points_ix: points_ix,
        };
        let item = PietItem::Fill(piet_fill);
        group.push_item(item, bbox);
    }

    pub fn path_stroke(
        &mut self,
        group: &mut GroupEncoder,
        bezpath: &BezPath,
        mut width: f32,
        mut rgba: u32,
    ) {
        // Fudge very thin lines to get better distance field rendering.
        if width < THIN_LINE {
            let alpha = (rgba & 0xff) as f32;
            // The sqrt here is to compensate for "correct" alpha blending.
            // We probably want a more systematic approach to stroke thickening.
            let alpha = alpha * (width / THIN_LINE).sqrt();
            rgba = (rgba & !0xff) | (alpha as u32);
            width = THIN_LINE;
        }
        let flattened = flatten_path(bezpath, TOLERANCE);
        for subpath in &flattened {
            self.polyline(group, subpath, width, rgba);
        }
    }

    pub fn fill_path(&mut self, group: &mut GroupEncoder, bezpath: &BezPath, rgba: u32) {
        let flattened = flatten_path(bezpath, TOLERANCE);
        for subpath in &flattened {
            self.fill_polyline(group, subpath, rgba);
        }
    }

    fn encode_points(&mut self, points: &[Point]) -> (Ref<f32>, Rect) {
        let mut bbox = None;
        for &pt in points {
            bbox = match bbox {
                None => Some(Rect::from_points(pt, pt)),
                Some(old_bbox) => Some(old_bbox.union_pt(pt)),
            };
        }
        let bbox = bbox.expect("encoded empty points vector");
        let points = points
            .iter()
            .map(|point| point_to_f32s(*point))
            .collect::<Vec<_>>();
        let points_ref = points.encode(&mut self.encoder);
        (points_ref.transmute(), bbox)
    }

    pub fn buf(&self) -> &[u8] {
        self.encoder.buf()
    }
}

impl GroupEncoder {
    fn push_item(&mut self, item: PietItem, rect: Rect) {
        self.bboxes.push([
            rect.x0.floor().max(0.0).min(65535.0) as u16,
            rect.y0.floor().max(0.0).min(65535.0) as u16,
            rect.x1.ceil().max(0.0).min(65535.0) as u16,
            rect.y1.ceil().max(0.0).min(65535.0) as u16,
        ]);
        self.items.push(item);
    }
}

fn point_to_f32s(point: Point) -> [f32; 2] {
    [point.x as f32, point.y as f32]
}
