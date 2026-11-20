# open-spro handoff, 2026-11-20

This document hands the espresso machine project to a fresh conversation. It describes the state of the work at the end of the session of 2026-11-20. The code is the authority where this document and the code disagree.

## What the project is

A DIY plumbed single-group, dual-boiler espresso machine with the capabilities of a Decent, a Slayer and a Kees van der Westen, published openly. Software is AGPL-3.0-or-later and hardware is CERN-OHL-S-2.0. The repository is open-spro. It holds a design document, a bill of materials, a hydraulic netlist, an Allium specification, CAD reference files, a Nix flake, and a PureScript (Halogen) simulator. Nearly all work in this session was on the simulator.

## Starting a new conversation

A new conversation starts with an empty workspace. It will not have any of these files. Upload the sources at the start, either as a catsrc dump with comments kept or as the individual files, and upload spec/machine.allium with them.

The assistant's sandbox could not reach the PureScript registry in this session. Dependencies were cloned from GitHub by version tag and compiled with purs directly, the bundle was made with esbuild, and the page was checked in headless Chrome with playwright. A new session has to rebuild that tooling before it can verify anything.

## Standing rules for this project

- Paste every code file in full in the message. File cards were used for most batches in this session because the batches were large and the context was short. That was a deviation each time, and the default has not changed.
- Allium files and markdown documents are delivered as files. Every file's destination is written in the message body.
- Update spec/machine.allium with every batch that changes machine behaviour, and run allium check and allium analyse to zero diagnostics before delivering.
- Builds must have zero warnings. The purs compiler reports warnings only for modules it recompiles, so delete the project's own output directories before the final build of a batch.
- Never write to the repository, and never offer to.
- No file may share a name with a file in another folder.

## Layout of the simulator

All paths are under sim/.

- src/Main.purs: mounts the page.
- src/Sim/Types.purs: enums, Step, Profile, Shot, Config (limits and setpoints), Tuning (calibration and loop gains), and their defaults.
- src/Sim/Plant.purs: the simulated machine. Lumped thermal and hydraulic model, the puck (dose, resistance, solubility, extraction), PlantConstants.
- src/Sim/Controller.purs: the pure controller. handle for events, tick for periodic work, setpoint ramps, the pump and mix laws, heater time-slicing, pacing.
- src/Sim/Curve.purs: the four ramp curves (jump, linear, ease, exponential).
- src/Sim/Invariants.purs: the spec's invariants evaluated on the live machine.
- src/Sim/World.purs: plant plus controller plus trace, scope samples, and the log of what the machine did.
- src/Sim/Params.purs: one table of every parameter with key, group, label, unit, range and step. Groups are control loops, calibration, limits, and physics. The settings panel and stored text come from this table.
- src/Sim/Profiles.purs: the three built-in shapes (classic 9 bar, line pressure pre-brew, declining).
- src/Sim/Shapes.purs: editing and storing the user's own shapes.
- src/Sim/Recipe.purs: beans and recipe. Ten roast levels with one development number each, roast starting points, puckOf, the house standard, samples, storage.
- src/Sim/Forecast.purs: runs the present recipe on a warm copy of the machine and reports time, weight, peak pressure, pacing used and extraction.
- src/Sim/Cup.purs: taste verdict from extraction, and the colour of the stream.
- src/Sim/Flavor.purs: the coffee map. Where a cup lands on the wheel, which notes it shows, and the route to a wanted note.
- src/Sim/Layout.purs: the tabs of the controls pane and the bench pane.
- src/Sim/Model.purs: page state and actions.
- src/Sim/Update.purs: pure state changes. Applying recipes, held settings, the grind search, choosing a roast, shape edits, parameter changes.
- src/Sim/Store.purs: localStorage.
- src/Sim/Problems.purs: what is wrong now, for red marking.
- src/Sim/App.purs: the Halogen component. Actions, persistence, the top bar, panes, gutters, the phone bar.
- src/Sim/View/Svg.purs: SVG helpers.
- src/Sim/View/Front.purs: the machine drawing.
- src/Sim/View/Inside.purs: the hydraulic schematic.
- src/Sim/View/Chart.purs: the live shot chart.
- src/Sim/View/Plots.purs: the forecast plot and the scope plot.
- src/Sim/View/Wheel.purs: the coffee map drawing.
- src/Sim/View/Widgets.purs: buttons, sliders, number boxes, tables.
- src/Sim/View/Panels.purs: machine pane, brew tab, cup tab, bench problems and readings.
- src/Sim/View/Bench.purs: profile designer, forecast line, scope, settings.
- test/Scenarios.purs: module Test.Main. Headless scenarios.
- dev/index.html: the page and all CSS.
- spago.yaml: dependencies. media-types and web-events are listed and no longer imported.

## What the simulator does now

The machine. Cold start, heating with time-sliced elements under a 1440 W budget, readiness with hysteresis, idle sleep, steam boiler fill, faults (over temperature, over pressure, mix over temperature, no inlet pressure, fill timeout, sensor failure) and service clearing.

Shots. A profile is a list of steps. Each step has a mode (line pre-brew, pressure, flow), a curve, a ramp time, targets, a longest duration and optional early exits on pressure or weight. Setpoints travel from where they stood at step entry to the step's target along the curve. Pressure setpoints are held 1 bar under the pressure limit.

Pacing. With a target time on the profile, the controller trims pressure by up to 3 bar in pressure steps to land the weight on time. It is in the spec with three invariants. It is a weak lever: 3 bar moves a shot by about two seconds.

Beans and recipe. A recipe records bean, roaster, roast date, roast level, dose, ratio, water temperature, shot time, grind steps and whether the shot is paced. The house standard is 22 g in, 44 g out, 30 s, Full City, grind 2.5. Dose, yield and time start held. Turning the roast drum moves every free setting to the roast's starting point and then searches for the grind that lands the shot on time.

Puck and cup. Puck resistance and solubility come from roast and grind. Each millilitre through the wet puck removes a share of what is left, scaled by solubility and by water temperature. A cup is balanced between 0.60 and 0.75 extracted, under-extracted below and over-extracted above.

Coffee map. The wheel from the kissaten press packet, with 24 notes in 8 families. Roast sets where a cup starts on the wheel and extraction shifts it. Clicking a note gives the route to it.

Interface. The design language of the press packet: paper and ink, square keys, deep green rules, burnt orange for the main action, numbered mono labels. Three panes: machine, controls (Brew, Cup, Profile, Settings) and a bench that can be put away (Problems, Readings, Scope). Gutters between panes drag to resize. Under 48rem wide the page shows one pane at a time with a bar at the bottom. Day and night themes.

## Storage keys in the browser

espresso-sim.recipes, espresso-sim.shapes, espresso-sim.params, espresso-sim.view, espresso-sim.theme, espresso-sim.locks, espresso-sim.sizes.

## Numbers that are tuning constants, not measurements

Every one of these was chosen to make the scenarios come out sensibly. None has been checked against a real shot.

- Puck resistance and solubility per roast, anchored at City (0.85, 0.72), Full City (1.0, 1.0) and French (1.3, 1.3), with straight lines between and beyond.
- Grind: each step coarser multiplies resistance by exp(-0.14) and solubility by exp(-0.05).
- Extraction per millilitre 0.032 and change per degree 0.04, relative to 93 C.
- The balanced band, 0.60 to 0.75.
- Roast starting points for temperature, ratio, time and grind. The three lightest roasts are extrapolated.
- Where each roast sits on the wheel (0.06 + 0.86 x development), how far extraction moves a cup (1.2 per unit of extraction), and the reach of 0.09.
- Pacing gain 0.6, filter 1.0 s, start at 6 g in the cup.

## Known gaps, in the order I would take them

1. Pre-infusion is not modelled beyond filling the dry puck. Add evenness as a puck state set while the puck wets (a gentle fill gives an even puck, and a fast fill followed by a steep ramp gives a channeled one that flows faster and extracts less), soak time (extraction into the water held in the puck, and cooling during the soak), a second axis on the coffee map, and advice that names pre-infusion. Every roast's starting point and every extraction scenario will need re-tuning afterwards. Spec open question 13 records this.
2. The profile designer is still a list of steps. A phase model (pre-brew, ramp, hold, decline) with handles on the forecast plot is the intended next form.
3. Pacing acts on pressure. Acting on flow would land the time almost exactly, with pressure as the result under a cap. Spec open question 11.
4. Roaster logo and label image saved with a recipe and shown as now brewing. It needs file upload and storage larger than localStorage.
5. The inside schematic has swinging valve gates and glowing elements, and is otherwise the original diagram.
6. Pre-shot cold-branch check. With the cold branch blocked the mix sensor reads about 101 C before the abort lands. A short flush to drain before each shot, or a mechanical limiter, is needed. Spec open question 8. This is a safety item for the real machine.
7. The pacing loop can nudge a shot the wrong way by a fraction of a second when it is already near its target.
8. Small items: the roast drum does not animate or take the scroll wheel, gutter dragging is mouse only, the wanted note is not saved, a saved recipe refers to a custom shape by position, the wheel in the designer is too small to read, the pushed samples show the held-setting advice, and the phone layout has been checked only in desktop Chrome at phone size.
9. The puck constants are calibrated at an 18 g reference dose, which is why the house standard sits at grind 2.5 and not at zero.

## Unanswered questions

- Pacing on pressure or on flow.
- A pre-shot flush or a mechanical limiter for the cold-branch hazard.
- Whether to adopt the Hylograph libraries for charts.
- Which parameters a service client may change on the real machine. Spec open question 10.

## Outside the simulator

docs/design.md, docs/bom.csv (5,701 USD, about 6,560 USD with contingency), docs/hydraulics.csv (netlist T01 to T43), cad/layout.step, cad/mix-manifold.step, cad/layout-parts.csv and flake.nix were delivered early in the session and have not changed since. The plan is to model the machine natively in FreeCAD using the STEP files as reference. None of that work has started.

## Verification state at handoff

The last full build had zero errors and zero warnings with every project module recompiled. All scenarios passed. allium check and allium analyse reported zero diagnostics on the spec delivered with this document. The last batch (portafilter, wand, valve gates, samples, gutters, phone layout) was checked in headless Chrome at 1480 by 900 and 390 by 844 with no console errors. The textures in the machine drawing were stand-ins on the assistant's side throughout.
