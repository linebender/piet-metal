//  Copyright 2019 The xi-editor authors.

//! Quick and dirty path flattening.

// A proper path flattening algorithm belongs in kurbo. In the meantime, this will let us
// get something rendered.

use kurbo::{BezPath, CubicBez, PathEl, Point};

pub fn flatten_path(path: &BezPath, tolerance: f64) -> Vec<Vec<Point>> {
    let mut result: Vec<Vec<Point>> = Vec::new();
    let mut cur_path = None;
    let mut last_pt = Point::default();
    for el in path.elements() {
        match el {
            PathEl::MoveTo(p) => {
                if let Some(sp) = cur_path.take() {
                    result.push(sp);
                }
                cur_path = Some(vec![*p]);
                last_pt = *p;
            }
            PathEl::LineTo(p) => {
                cur_path.as_mut().unwrap().push(*p);
                last_pt = *p;
            }
            PathEl::CurveTo(p1, p2, p3) => {
                let cb = CubicBez::new(last_pt, *p1, *p2, *p3);
                // This is a really hacky way to get finer subdivision. It will
                // give overly coarse results if the Bézier is close to cubic. But
                // close enough for now.
                //
                // A reasonable approach would be to subdivide the quads based
                // on the true error, or we could try to do a fancier algorithm.
                for (_, _, q) in cb.to_quads(tolerance * 1e-2) {
                    cur_path.as_mut().unwrap().push(q.p2);
                }
                last_pt = *p3;
            }
            _ => (),
        }
    }
    if let Some(sp) = cur_path.take() {
        result.push(sp);
    }
    result
}
