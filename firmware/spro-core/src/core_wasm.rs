//! The controller as a wasm module, for the simulator.
//!
//! The module holds one controller and one buffer. A caller writes a
//! message into the buffer, calls `call`, and reads the status back
//! from the same buffer. The controller's whole state is one block of
//! bytes the caller may copy out and copy back in, which is how the
//! simulator keeps many machines at once (the one on the page, and
//! every forecast shot) with a single module.
//!
//! This file is the only place in the crate with `unsafe` in it. It is
//! compiled for wasm32 only, where there is one thread.

use core::mem::{size_of, MaybeUninit};
use core::ptr::addr_of_mut;

use crate::core_control::Controller;
use crate::core_types::Reject;
use crate::core_wire::{apply, init, write_status, BUFFER_LEN, OP_INIT, STATUS_LEN};

static mut CONTROLLER: MaybeUninit<Controller> = MaybeUninit::uninit();
static mut BUFFER: [f64; BUFFER_LEN] = [0.0; BUFFER_LEN];
/// Whether `CONTROLLER` holds a controller yet.
static mut READY: bool = false;

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

/// Where the message buffer is, and how many numbers it holds.
#[no_mangle]
pub extern "C" fn buffer_ptr() -> *mut f64 {
    addr_of_mut!(BUFFER) as *mut f64
}

#[no_mangle]
pub extern "C" fn buffer_len() -> usize {
    BUFFER_LEN
}

/// How many numbers of the buffer a status fills.
#[no_mangle]
pub extern "C" fn status_len() -> usize {
    STATUS_LEN
}

/// Where the controller's state is, and how many bytes it is. The bytes
/// mean something only to the module they came from. A caller that
/// copies a saved state in must then call `state_loaded`.
#[no_mangle]
pub extern "C" fn state_ptr() -> *mut u8 {
    addr_of_mut!(CONTROLLER) as *mut u8
}

#[no_mangle]
pub extern "C" fn state_loaded() {
    // SAFETY: one thread, and nothing else holds the flag.
    unsafe { *addr_of_mut!(READY) = true };
}

#[no_mangle]
pub extern "C" fn state_len() -> usize {
    size_of::<Controller>()
}

/// Run one operation on the message in the buffer and leave the status
/// in the buffer. Returns 0, or the number of the reason the operation
/// was refused. See `core_wire::apply` for the operations.
///
/// Every operation but `OP_INIT` works on the state in `state_ptr`.
/// Until `OP_INIT` has succeeded or a saved state has been loaded there
/// is no controller, and every other operation is refused as malformed
/// with the buffer left as it was. A state that is copied in must be
/// bytes this same module produced.
#[no_mangle]
pub extern "C" fn call(op: u32, a: f64, b: f64) -> u32 {
    // SAFETY: wasm32 has one thread and `call` does not call itself, so
    // nothing else holds these statics while it runs.
    let buffer: &mut [f64; BUFFER_LEN] = unsafe { &mut *addr_of_mut!(BUFFER) };
    let controller: &mut MaybeUninit<Controller> = unsafe { &mut *addr_of_mut!(CONTROLLER) };
    let ready: &mut bool = unsafe { &mut *addr_of_mut!(READY) };
    let outcome = if op == OP_INIT {
        init(&buffer[..]).map(|made| {
            controller.write(made);
            *ready = true;
        })
    } else if *ready {
        // SAFETY: `ready` says a controller has been written here.
        apply(unsafe { controller.assume_init_mut() }, op, a, b, &buffer[..])
    } else {
        Err(Reject::Malformed)
    };
    if *ready {
        // SAFETY: as above.
        write_status(unsafe { controller.assume_init_ref() }, &mut buffer[..STATUS_LEN]);
    }
    match outcome {
        Ok(()) => 0,
        Err(reason) => reason as u32,
    }
}

/// The words for a refusal, as UTF-8 at `reject_ptr` of length
/// `reject_len`. An unknown number has no words.
fn reject_text(code: u32) -> &'static str {
    Reject::ALL
        .iter()
        .find(|reason| **reason as u32 == code)
        .map_or("", |reason| reason.text())
}

#[no_mangle]
pub extern "C" fn reject_ptr(code: u32) -> *const u8 {
    reject_text(code).as_ptr()
}

#[no_mangle]
pub extern "C" fn reject_len(code: u32) -> usize {
    reject_text(code).len()
}
