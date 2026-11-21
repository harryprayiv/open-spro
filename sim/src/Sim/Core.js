// The controller core, loaded from wasm.
//
// This is the only hand-written JavaScript in the simulator. PureScript
// cannot call into a wasm module by itself, so this file does the three
// things that need the host: it instantiates the module, copies bytes
// and numbers in and out of its memory, and calls it. It holds no logic
// of the machine. What the numbers mean is decided in the Rust crate
// (firmware/spro-core/src/core_wire.rs) and read in Controller.purs.

let loaded = null;

// The module is instantiated once, the first time it is needed.
const load = (base64) => {
  if (loaded === null) {
    const bytes = Uint8Array.from(atob(base64), (c) => c.charCodeAt(0));
    const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes), {});
    // `current` is the last state the module handed out. When the
    // caller passes that same state back, the module already holds it
    // and there is nothing to copy in.
    loaded = { exports: instance.exports, current: null };
  }
  return loaded;
};

// A state that belongs to no controller yet. Only `OP_INIT` accepts it.
export const noState = new Uint8Array(0);

// Run one operation. `state` is never changed: the new state is a fresh
// copy, so to PureScript this is a pure function.
export const callImpl = (base64) => (state) => (op) => (a) => (b) => (input) => {
  const m = load(base64);
  const x = m.exports;
  const memory = x.memory.buffer;
  const buffer = new Float64Array(memory, x.buffer_ptr(), x.buffer_len());
  const held = new Uint8Array(memory, x.state_ptr(), x.state_len());
  if (state.length !== 0 && state !== m.current) {
    held.set(state);
    x.state_loaded();
  }
  buffer.set(input);
  const code = x.call(op, a, b);
  const next = held.slice();
  m.current = next;
  return { code, state: next, status: Array.from(buffer.subarray(0, x.status_len())) };
};

// How many numbers one message may hold.
export const bufferLenImpl = (base64) => load(base64).exports.buffer_len();

// The module's own words for a refusal.
export const rejectTextImpl = (base64) => (code) => {
  const x = load(base64).exports;
  const bytes = new Uint8Array(x.memory.buffer, x.reject_ptr(code), x.reject_len(code));
  return new TextDecoder().decode(bytes);
};
