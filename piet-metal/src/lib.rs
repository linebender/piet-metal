//  Copyright 2019 The xi-editor authors.

use std::mem;
use std::ptr::copy_nonoverlapping;
use std::str::FromStr;

use kurbo::{BezPath, Circle, Line, Point, Rect, Shape, Vec2};

use roxmltree::Document;

mod flatten;
mod scene;

use scene::{GroupEncoder, SceneEncoder};

#[allow(unused)]
fn make_cardioid(encoder: &mut SceneEncoder, group: &mut GroupEncoder) {
    let n = 97;
    let dth = std::f64::consts::PI * 2.0 / (n as f64);
    let center = Point::new(1024.0, 768.0);
    let r = 750.0;
    for i in 1..n {
        let p0 = center + Vec2::from_angle(i as f64 * dth) * r;
        let p1 = center + Vec2::from_angle(((i * 2) % n) as f64 * dth) * r;
        encoder.circle(group, &Circle::new(p0, 8.0));
        encoder.stroke_line(group, Line::new(p0, p1), 2.0, 0x000080e0);
    }
}

#[allow(unused)]
fn make_path_test(encoder: &mut SceneEncoder, group: &mut GroupEncoder) {
    encoder.fill_polyline(
        group,
        &[
            Point::new(10.0, 10.0),
            Point::new(15.0, 800.0),
            Point::new(300.0, 500.0),
        ],
        0x80e0,
    );
}

fn make_tiger(encoder: &mut SceneEncoder, group: &mut GroupEncoder) {
    let scale = 8.0;
    let tiger_svg = include_bytes!("../Ghostscript_Tiger.svg");
    let doc = Document::parse(std::str::from_utf8(tiger_svg).unwrap()).unwrap();
    let root = doc.root_element();
    let g = root.first_element_child().unwrap();
    for path in g.children() {
        if path.is_element() {
            let d = path.attribute("d").unwrap();
            let bez_path = BezPath::from_svg(d);
            if let Ok(ref bp) = bez_path {
                let xform_path = kurbo::Affine::scale(scale) * bp;
                if let Some(fill_color) = path.attribute("fill") {
                    encoder.fill_path(group, &xform_path, parse_color(fill_color));
                }
                if let Some(stroke_color) = path.attribute("stroke") {
                    let width = f32::from_str(path.attribute("stroke-width").unwrap()).unwrap();
                    let width = width * (scale as f32);
                    let color = parse_color(stroke_color);
                    encoder.path_stroke(group, &xform_path, width, color);
                }
            }
        }
    }
}

fn parse_color(color: &str) -> u32 {
    if color.as_bytes()[0] == b'#' {
        let mut hex = u32::from_str_radix(&color[1..], 16).unwrap();
        if color.len() == 4 {
            hex = (hex >> 8) * 0x110000 + ((hex >> 4) & 0xf) * 0x1100 + (hex & 0xf) * 0x11;
        }
        (hex << 8) + 0xff
    } else {
        0xff00ff80
    }
}

#[no_mangle]
pub unsafe extern "C" fn init_test_scene(scene_buf: *mut u8, buf_size: usize) {
    let buf_slice = std::slice::from_raw_parts_mut(scene_buf, buf_size);
    let mut scene_encoder = SceneEncoder::new();
    let mut group = GroupEncoder::default();
    //make_cardioid(&mut scene_encoder, &mut group);
    //make_path_test(&mut scene_encoder, &mut group);
    make_tiger(&mut scene_encoder, &mut group);
    scene_encoder.encode_root_group(group);
    let buf = scene_encoder.buf();
    buf_slice[..buf.len()].copy_from_slice(buf);
}
