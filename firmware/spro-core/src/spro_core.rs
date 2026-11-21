//! The open-spro controller core.
//!
//! This crate is the firmware's behaviour with no hardware in it: the
//! rules of `spec/machine.allium` and the control laws the spec leaves
//! open. It reads a `Sensors` value and writes an `Actuators` value and
//! touches nothing else, so the same code runs in three places: linked
//! into the ESP32 firmware, built for wasm32 and loaded by the
//! simulator, and on the host under `cargo test`.
//!
//! It uses no allocator and no standard library. Every number is an
//! `f32` and every time is a count of milliseconds, on every target.
//!
//! On the microcontroller and in wasm the crate is built without the
//! standard library, which is what proves it does not need one. On the
//! host it is built with it, for the test harness only.
#![cfg_attr(any(target_arch = "wasm32", target_os = "none"), no_std)]

pub mod core_control;
pub mod core_curve;
pub mod core_types;
pub mod core_wire;

#[cfg(target_arch = "wasm32")]
pub mod core_wasm;

#[cfg(test)]
mod core_tests;

pub use core_control::Controller;
pub use core_types::*;
