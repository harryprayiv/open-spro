# Open-Spro

An open espresso machine: the hardware design, the controller that runs it, and a simulator that runs the same controller in a browser.

**[Launch the simulator](https://harryprayiv.github.io/open-spro/)**

Nothing needs installing. The page is static and runs entirely in your browser.

## Status

The machine has not been built yet. What exists today is the design, a specification of how the machine must behave, the controller core, and the simulator. The simulator is where the controller is being proven before any metal is cut.

## The simulator

The simulator shows the front of the machine, its plumbing, and a chart of the shot. A model of the boilers, pump, valves and coffee puck stands in for the hardware. The controller making the decisions is the real one: the Rust core that will run on the microcontroller, compiled to WebAssembly.

To pull a shot:

1. Press **Wake, brew and steam**.
2. Press **Skip warm-up**, or wait for the machine to heat.
3. Press the round button on the group to run the profile.

Things to try once a shot is running:

- Drag the fader. With the selector on pressure, the fader sets the brew pressure by hand and the profile's steps stand still. The **Follow the profile** button gives the fader back.
- Click the knurled wheel on the left of the machine to turn the selector. On temperature the fader sets the water temperature while the profile carries on. On time the fader only shows how far the shot has run.
- Change the beans, the grind and the recipe on the Brew tab, and see what the forecast and the Cup tab say the cup will taste like.
- Open the bench and break something: cut the water supply, block the cold branch, or unplug the link between the page and the machine.
- Drag the thin lines between the panes and drawings to resize them.

## What is in the repository

| Path | What it holds |
|---|---|
| `sim/` | The simulator, written in PureScript with Halogen |
| `firmware/spro-core/` | The controller core, written in Rust with no standard library. It builds for the ESP32-C6 and for WebAssembly from the same source |
| `.inference/machine.allium` | The specification of the machine's behaviour, in Allium |
| `docs/` | The design notes, the bill of materials and the hydraulics |
| `flake.nix` | The development shells and the build of the simulator's site |

## Building

Everything is built with Nix. There is no npm anywhere in the project.

Build the simulator's site, which also runs its test scenarios:

```
nix build .#demo
```

The site is then in `result/`. Open `result/dev/index.html` in a browser.

To work on the code, enter the development shell:

```
nix develop .#code
```

Inside it, from `sim/`, `spago test` runs the scenarios and `spago bundle` writes `sim/dev/app.js`. From `firmware/spro-core/`, `cargo test` runs the controller's own tests. After changing the controller, `nix run .#core-wasm` rebuilds the WebAssembly the simulator loads.

## Licence

The software is licensed under AGPL-3.0-or-later and the hardware design under CERN-OHL-S-2.0. The full texts are in `LICENSES/`.
