//  Copyright 2019 The xi-editor authors.

use std::mem;
use std::ptr::copy_nonoverlapping;

use kurbo::{Circle, Line, Vec2, Rect, Shape};

// Keep these in sync with PietShaderTypes.h

#[repr(C)]
#[derive(Clone, Copy)]
struct SimpleGroup {
    n_items: u32,
    items_ix: u32,
}

#[repr(C)]
#[derive(Clone, Copy, Default)]
struct ShortBbox([u16; 4]);

#[repr(C)]
union PietItem {
    circle: PietCircle,
    stroke_line: PietStrokeLine,
    fill: PietFill,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct PietCircle {
    item_type: ItemType,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct PietStrokeLine {
    item_type: ItemType,
    flags: u32,
    rgba: u32,
    width: f32,
    start: (f32, f32),
    end: (f32, f32),
}

#[repr(C)]
#[derive(Clone, Copy)]
struct PietFill {
    item_type: ItemType,
    flags: u32,
    rgba: u32,
    n_points: u32,
    points_ix: u32,
}

#[repr(u32)]
#[derive(Clone, Copy)]
enum ItemType {
    Circle = 1,
    Line = 2,
    Fill = 3,
}

pub struct Encoder<'a> {
    buf: &'a mut [u8],
    free_space: usize,
    group_count: usize,
    group_ix: usize,
    // Start index of currently open group.
    group_start: usize,

}

impl ShortBbox {
    fn from_rect(rect: Rect) -> ShortBbox {
        ShortBbox([
            rect.x0.floor().max(0.0).min(65535.0) as u16,
            rect.y0.floor().max(0.0).min(65535.0) as u16,
            rect.x1.ceil().max(0.0).min(65535.0) as u16,
            rect.y1.ceil().max(0.0).min(65535.0) as u16,
        ])
    }
}

fn vec2_to_f32s(point: Vec2) -> (f32, f32) {
    (point.x as f32, point.y as f32)
}

impl<'a> Encoder<'a> {
    pub fn new(buf: &mut [u8]) -> Encoder {
        Encoder {
            buf,
            free_space: 0,
            group_count: 0,
            group_start: 0,
            group_ix: 0,
        }
    }

    pub fn alloc(&mut self, size: usize) -> usize {
        let result = self.free_space;
        self.free_space += size;
        result
    }

    // It's probably better to do this without unsafety (after all, we're just creating bytes).
    // Probably the thing to do is write proc macros.
    pub unsafe fn write_struct<T>(&mut self, ix: usize, s: &T) {
        let len = mem::size_of::<T>();
        println!("writing {} bytes at {}", len, ix);
        copy_nonoverlapping(s as *const T as *const u8, self.buf[ix..ix + len].as_mut_ptr(), len);
    }

    pub fn begin_group(&mut self, n_items: usize) {
        let item_start = mem::size_of::<SimpleGroup>() + n_items * mem::size_of::<ShortBbox>();
        let total_size = item_start + n_items * mem::size_of::<PietItem>();
        self.group_start = self.alloc(total_size);
        self.group_count = n_items;
        let group = SimpleGroup {
            n_items: n_items as u32,
            items_ix: (self.group_start + item_start) as u32,
        };
        unsafe {
            self.write_struct(self.group_start, &group);
        }
    }

    pub fn end_group(&mut self) {
        assert_eq!(self.group_ix, self.group_count);
        // This will get more interesting when we have nested groups.
    }

    unsafe fn add_item<T>(&mut self, item: &T, bbox: ShortBbox) {
        assert!(self.group_ix < self.group_count);
        let bbox_ix = self.group_start + mem::size_of::<SimpleGroup>() 
            + self.group_ix * mem::size_of::<ShortBbox>();
        self.write_struct(bbox_ix, &bbox);
        let item_ix = self.group_start + mem::size_of::<SimpleGroup>()
            + self.group_count * mem::size_of::<ShortBbox>()
            + self.group_ix * mem::size_of::<PietItem>();
        self.write_struct(item_ix, item);
        self.group_ix += 1;
    }

    // Encode a circle. Currently this has a lot of limitations and is mostly used for debugging
    // and performance analysis, but could be expanded to the real thing.
    pub fn circle(&mut self, circle: &Circle) {
        let piet_circle = PietCircle {
            item_type: ItemType::Circle,
        };
        unsafe {
            self.add_item(&piet_circle, ShortBbox::from_rect(circle.bounding_box()));
        }
    }

    // Should these be by reference or move?
    pub fn stroke_line(&mut self, line: Line, width: f32, rgba: u32) {
        let piet_stroke_line = PietStrokeLine {
            item_type: ItemType::Line,
            flags: Default::default(),
            rgba: rgba.to_be(),
            width,
            start: vec2_to_f32s(line.p0),
            end: vec2_to_f32s(line.p1),
        };
        unsafe {
            self.add_item(&piet_stroke_line, ShortBbox::from_rect(line.bounding_box()));
        }
    }

    // Signature will change, need to deal with subpaths and also want curves.
    pub fn fill(&mut self, points: &[Vec2], rgba: u32) {
        let mut points_ix = self.alloc(points.len() * mem::size_of::<(f32, f32)>());
        let piet_fill = PietFill {
            item_type: ItemType::Fill,
            flags: Default::default(),
            rgba: rgba.to_be(),
            n_points: points.len() as u32,
            points_ix: points_ix as u32,
        };
        let mut bbox = None;
        for &pt in points {
            bbox = match bbox {
                None => Some(Rect::from_points(pt, pt)),
                Some(old_bbox) => Some(old_bbox.union_pt(pt)),
            };
            unsafe {
                self.write_struct(points_ix, &vec2_to_f32s(pt));
                points_ix += mem::size_of::<(f32, f32)>();
            }
        }
        if let Some(bbox) = bbox {
            unsafe {
                self.add_item(&piet_fill, ShortBbox::from_rect(bbox));
            }
        } else {
            panic!("fill with 0 points");
        }
    }

    #[allow(unused)]
    fn debug_print(&self) {
        unsafe {
            for i in (0..self.free_space).step_by(4) {
                println!("{:04x}: {:08x}", i, std::ptr::read((self.buf.as_ptr().add(i) as *const u32)));
            }
        }
    }
}

#[allow(unused)]
fn make_cardioid(encoder: &mut Encoder) {
    let n = 97;
    let dth = std::f64::consts::PI * 2.0 / (n as f64);
    let center = Vec2::new(1024.0, 768.0);
    let r = 750.0;
    encoder.begin_group((n - 1) * 2);
    for i in 1..n {
        let p0 = center + Vec2::from_angle(i as f64 * dth) * r;
        let p1 = center + Vec2::from_angle(((i * 2) % n) as f64 * dth) * r;
        encoder.circle(&Circle::new(p0, 8.0));
        encoder.stroke_line(Line::new(p0, p1), 2.0, 0x000080e0);
    }
    encoder.end_group();
}

#[allow(unused)]
fn make_path_test(encoder: &mut Encoder) {
    encoder.begin_group(1);
    encoder.fill(&[Vec2::new(10.0, 10.0), Vec2::new(15.0, 800.0), Vec2::new(300.0, 500.0)], 0x80e0);
    encoder.end_group();
}

fn make_test_scene(encoder: &mut Encoder) {
    //make_cardioid(encoder);
    make_path_test(encoder);
}

#[no_mangle]
pub unsafe extern fn init_test_scene(scene_buf: *mut u8, buf_size: usize) {
    let buf_slice = std::slice::from_raw_parts_mut(scene_buf, buf_size);
    let mut encoder = Encoder::new(buf_slice);
    make_test_scene(&mut encoder);
    encoder.debug_print();
}
