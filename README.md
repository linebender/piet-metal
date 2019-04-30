# piet-metal

This repository is currently an experiment in using GPU compute to implement the piet 2D graphics API. In its initial stages it is an Objective-C macOS application, to make it easier to use Xcode tools. When it becomes more functional, Rust bindings will be added, with the Objective-C code for the Metal bindings built from the Rust library's build.rs.

## Following along

I'm discussing the design and my findings in the [#druid] stream on xi Zulip. I've also creates a [notes document].

## License

Licensed under either of these:

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   https://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   https://opensource.org/licenses/MIT)

[notes document]: https://docs.google.com/document/d/1LILagXyJgYtlm6y83x1Mc2VoNfOcvW_ZiCldZbs4yO8/edit?usp=sharing
[#druid]: https://xi.zulipchat.com/#narrow/stream/147926-druid
