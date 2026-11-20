# Espresso machine build: design baseline

Status: second draft, 2026-11-20. Revised so that water temperature can be profiled during a shot. Nothing here has been built or tested. Prices marked "estimate" in bom.csv are from general knowledge and need quotes. The Allium spec in spec/machine.allium was written without access to the Allium checker and must be run through `allium check` before it is trusted.

## 1. What this machine is

A plumbed, single group, dual boiler machine on one 120 V 15 A circuit. Pressure, flow and water temperature are all programmable per step of a shot. It has a heated group, a speed-controlled pump, a Slayer style restricted pre-brew stage on line pressure, a hot and cold mixing stage at the group, a scale under the cup, and a controller that can be woken and monitored from a phone.

## 2. Requirements that conflict, and how each is resolved

**Temperature profiling against thermal mass.** The first draft used a saturated group, where boiler water fills a heavy group and the mass holds temperature steady. That design cannot change temperature during a shot. This draft replaces it with the arrangement Decent uses, built in stainless: a brew boiler held hotter than any brew temperature, a cold branch, and a mixing point right at the group. The group block is still heated, with its own sensor, but it no longer carries the water. The water runs through a thin-wall insert with an air gap to the block, so the metal it touches weighs a few grams and follows the water temperature quickly.

What this trade costs you:

- Stability now comes from a control loop and a calibration, where a saturated group gets it from mass. A badly tuned loop gives a worse shot than a Linea. A well tuned one gives the same stability plus profiling.
- The basket, the coffee and the water already sitting above the puck still have thermal mass. A step change at the mix point reaches the puck over a few seconds. No machine avoids this, Decent included.
- At very low flow, such as a soaking pre-brew, the mix sensor reads nearly stagnant water and the loop has little to work with. The firmware falls back to a calibrated valve position there.
- The reservoir runs at about 104 C. If the cold branch fails during a shot, water hotter than boiling point at open pressure heads for the puck. The spec aborts the shot on mix temperature above 99 C, and the reservoir setpoint is kept low to limit how violent that failure can be.
- Two more custom machined parts (mix manifold, water insert) and one more actuator.

**No plastics anywhere against how valves and pumps seal.** Every solenoid valve, check valve, group gasket and boiler flange seals with an elastomer or PTFE. A machine with zero polymer in the water path does not exist, including the Slayer and the Speedster. The rule this design can meet is: no structural plastic, no plastic tube, no plastic fittings, no thread tape; metal is 316 or 304 stainless throughout; polymers appear only as food grade seals and in one named exception, the pump gears.

**The pump exception.** A magnetically coupled gear pump gives smooth, pulse-free flow that is controllable down to near zero, which is what profiling needs. Its gears are PEEK. The alternative with no polymer is a stainless rotary vane pump with graphite vanes on a variable speed motor, which is heavier, louder and controls poorly at pre-brew flow rates. The gear pump sits on the cold side at inlet temperature. I recommend accepting PEEK there. If you refuse, the vane pump path stays open and only the pump, its motor drive and its mounting change.

**Strong steam against 1440 W continuous.** A 15 A circuit carries 1800 W peak and 1440 W continuous. Brew element 600 W, steam element 1000 W, group heaters 150 W, pump and electronics about 80 W. They cannot all run together. The firmware time-slices the elements so the total never exceeds the budget, with priority: brew reservoir during a shot, steam boiler otherwise. A 1.5 to 2 L steam boiler at 1.8 bar stores enough energy that steaming one or two pitchers does not depend on the element keeping up. Mixing does not raise the energy a shot needs: the water still has to be heated from inlet temperature to brew temperature, and a hot reservoir blended with cold delivers the same heat as a cooler one used neat. Cold start to fully ready will take roughly 20 to 25 minutes, which is why remote wake matters.

**Battery.** The 24 V bus already runs the pump, valves, group heaters and logic. The boiler elements need about 1.5 kW, which means a commercial LiFePO4 power station with an 1800 W inverter. Nothing in the machine needs to change for that, so it is deferred.

## 3. Hydraulics

The tube-by-tube netlist is hydraulics.csv. The machine has three stages in series: a pressure stage, a temperature stage, and the group.

**Inlet.** Bulkhead, ball valve, strainer, regulator set to 3 bar, then a cold manifold carrying the inlet pressure transducer, an inlet temperature sensor and a conductivity cell.

**Pressure stage, pump branch.** Cold manifold, flex hose, gear pump, flex hose, check valve, brew tee. The pump runs closed loop on brew pressure or on its own speed.

**Pressure stage, bypass branch (the Slayer part).** Cold manifold, 2-way solenoid SV_BYP, needle valve NV_PRE, check valve, brew tee. With the pump stopped and SV_BYP open, regulated line pressure pushes water through the needle valve at a flow you set by hand. This needs no control loop.

**Brew manifold.** After the brew tee: brew pressure transducer and the 12 bar expansion valve. The expansion valve is mechanical. It relieves thermal expansion and caps brew pressure if the firmware fails. Both the pump and the bypass arrive here, so everything downstream behaves the same whichever one is supplying water.

**Temperature stage.** The brew manifold splits in two. The hot branch goes into the bottom of the brew boiler, out of the top, and through a short insulated thin-wall stub to the mix manifold, entering through a 0.8 mm jet. The cold branch goes through the motorised needle valve MV_MIX and a check valve to the same mix manifold. The two streams meet in a static mixer with a fast RTD immediately after it. Opening MV_MIX sends more cold water and lowers the temperature. The jet on the hot side is there so the valve has a restriction to work against. Because the split is downstream of the brew manifold, temperature control works during line pressure pre-brew as well as under the pump.

**Group.** The mix manifold bolts to the 3-way valve SV_GRP, which feeds the thin-wall insert, which ends at the dispersion plate. A second fast RTD sits above the screen; that is the reading closest to the coffee. The block around the insert has two 24 V cartridge heaters and its own RTD. Its job is to hold the portafilter, basket and insert surroundings at the base temperature of the selected profile, so the water loses as little as possible on the way through. Keep the volume between the mix point and the screen under about 5 ml; at 2 ml per second that is a 2.5 second transport delay, and it grows as flow falls.

**Steam side.** The steam boiler fills on line pressure through a check valve and SV_FILL; 3 bar line pressure is above the 1.8 bar steam setpoint, so no pump is involved. On top: certified safety valve, vacuum breaker, and a pressure transducer on a cooling loop. Steam pressure is controlled from the transducer. The steam valve is manual. A ball valve at the bottom is for blowdown.

**Where each valve type is used.** Solenoids for anything the controller must sequence: bypass, fill, group vent. A hand needle valve for the pre-brew restriction. A motorised needle valve for the temperature blend. Ball valves for isolation and drains that a person operates during service. Check valves wherever two pressure sources or two temperatures meet. Spring valves (expansion, safety) for protection that must not depend on software.

**If the valve does not control well enough.** The alternative is Decent's own method: a second gear pump on the cold branch in place of MV_MIX, so each stream has a known flow and the mix temperature can be computed in advance. It costs about 450 more and the line pressure pre-brew stage loses temperature control, because a stopped gear pump blocks the cold branch. Phase 2 tests the valve on the bench before any group part is machined. Design the mix manifold so its cold port accepts either.

**Fittings.** Use 316 tube with two-ferrule compression fittings from McMaster. They need no brazing, no sealant, and can be remade. Espresso components are almost all BSPP threads (G 1/8, G 1/4, G 3/8) and McMaster stock is mostly NPT. Mixing them leaks. Pick tube compression as the standard and buy a BSPP adapter for each component port, sealed with a copper or bonded washer.

## 4. Scale prevention

Scale comes from calcium and magnesium carbonate dropping out of hot water. Three measures, in order of effect:

1. Feed water with almost no calcium or magnesium and a little bicarbonate. Bicarbonate keeps the water from attacking metal and keeps it conductive enough for the level probe. Chloride must stay very low because it pits stainless at boiler temperature. What produces that water is upstream of the bulkhead and is your later project; the machine only needs to measure it.
2. The conductivity cell at the inlet raises a warning when the feed water is out of range.
3. The steam boiler only loses pure vapour, so whatever minerals arrive stay and concentrate. The firmware counts fills and asks for a blowdown through the drain ball valve after a set number.

A reservoir at 104 C scales faster than one at 93 C on the same water, which makes point 1 more important in this draft. The 0.8 mm jet on the hot branch is the first place scale would show up. Pure reverse osmosis or distilled water is wrong for this machine: the level probe cannot see it and it is corrosive.

## 5. Sensing

Temperature: five PT100 class A sensors, 4 wire, wired in series on one excitation current and read by one delta-sigma ADC against a reference resistor. Three are ordinary sheathed probes (brew reservoir, group block, cold inlet). Two are 1.5 mm fast probes (mix manifold outlet, above the screen). The steam boiler has no temperature sensor; it is controlled on pressure and protected by its thermal cutoff and safety valve. Pressure: three 316L ratiometric transducers on a separate I2C ADC; the ESP32's internal ADC is too nonlinear and noisy to use. Weight: one single-point load cell under a cup platform that is mechanically separate from the drip tray, read at 80 samples per second. Flow: computed from pump speed with a slip correction against pressure, checked against the scale; there is no flow meter, which removes a plastic impeller from the water path.

## 6. Electrical safety chain

This chain works with the controller dead or wrong:

1. GFCI protection at the mains entry and a fused inlet.
2. A manual-reset thermal cutoff on each boiler in series with its element.
3. A safety contactor that carries both boiler elements. Its coil is fed through the thermal cutoffs and through a HEAT_ENABLE output that the controller must keep toggling; a hung controller drops the contactor.
4. Solid state relays do the regulation, downstream of the contactor. SSRs fail shorted, which is why they are never the only switch.
5. Expansion valve and steam safety valve on the water side.
6. Every chassis panel bonded to earth.

Buy the steam boiler as a manufacturer-rated spare part. A home-welded steam vessel is the one item in this project that can injure someone with no warning. Hydrostatically test each assembled pressure circuit cold at 1.5 times its working pressure before it is ever heated. This machine will carry no safety listing; check what that means for your household insurance.

## 7. Control architecture

**Common commercial practice.** Machines from La Marzocco, Slayer and most Italian makers run C firmware on a vendor control board and rely on the mechanical chain above for safety. Decent pairs a microcontroller with a tablet app. Community projects such as Gaggiuino run Arduino C++ on an STM32.

**Safety-critical practice.** Avionics separates a small, analysable control core from everything connected to a network, specifies behaviour before code, and puts independent monitors beside the control law. NASA's Copilot is a Haskell-embedded stream language that generates constant-memory C monitors for exactly that purpose.

**This design.** Two computers with one job each.

- Microcontroller, ESP32-C6, firmware in Rust with embassy and esp-hal. It owns every sensor and actuator, runs the heater, pump and mix loops, executes a shot profile, enforces the interlocks in the Allium spec, and feeds the hardware watchdog. With the Pi unplugged it still heats and pulls a default profile from the paddle. The C6 is RISC-V, so it builds with the upstream Rust toolchain and needs no forked compiler in the flake. It also has Thread and Matter radios for later Home Assistant work.
- Raspberry Pi on NixOS. A Haskell service stores profiles and shot logs, schedules wake-ups, and serves a PureScript Halogen page built from the same component set as nixtune. It talks to the microcontroller over an isolated UART with COBS-framed CBOR messages. The Pi sends requests and the microcontroller checks each one against its interlocks before acting.

**Pin budget.** The mix stage adds a stepper and two sensors. To stay on the C6: the stepper driver is an SPI motion controller that homes by stall detection (one chip select, no step, direction or limit pins), all five RTDs share one ADC, the pressure transducers and load cell are on I2C, and the paddle inputs move to an I2C expander. That uses 21 of roughly 23 available pins. Two spare pins is thin. If the board needs more during KiCad layout, move to the ESP32-S3 and accept its forked Rust toolchain in the flake.

**Juniper.** I recommend against it. It is an ML-family language closer to F# than to Haskell, its compiler is an F# program, it emits Arduino C++, and it has essentially one maintainer and no driver ecosystem. This firmware switches mains heaters on pressure vessels and needs maintained HAL crates, async timers and a debugger. If you want Haskell on the firmware side, the place for it is Copilot: write the safety monitors as Copilot streams, generate C, and link that C into the Rust firmware as an independent check on the control code.

**Control flow.** At 100 Hz: read ADCs, update filters, run supervision (limits, sensor plausibility), run the pump loop and the mix loop. At 10 Hz: run the reservoir and group block temperature loops and allocate mains half-cycles to elements within the watt budget. Event driven: mode changes, fill sequence, shot step transitions.

**Mix loop.** Feed-forward first: from inlet temperature, reservoir temperature and the target, compute the cold fraction needed, then look up the valve position for that fraction at the current flow from a calibration table made on the bench. Feedback second: the mix RTD trims the position. A third, slow term compares the above-screen RTD with the mix RTD and raises the mix target to cover the loss through the insert. Below a flow threshold the feedback terms freeze and the valve holds the feed-forward position.

**Group block loop.** The block RTD controls the cartridge heaters to the selected profile's base temperature. Changing profile changes that setpoint and the block needs a few minutes to follow; the firmware refuses to start a shot until it has.

**Reservoir loop.** Holds 104 C, with feed-forward from hot-branch flow so the element starts before the temperature sags.

## 8. Mechanical

FreeCAD: model the hydraulic skeleton first (boilers, group, pump, manifolds) and route every tube in hydraulics.csv before designing a single panel, because tube bend radii and fitting wrench clearance decide the footprint. Keep electronics in a separate compartment behind a bulkhead, above the floor of the machine, with a drip shield, and keep the load cell wiring away from the SSRs, the stepper and pump leads. The enclosure is laser cut and bent 304 stainless. Passive cup warming comes from the steam boiler's top panel.

The group is now three custom parts: the heated block, the thin-wall water insert, and the mix manifold. A used saturated group from a La Marzocco or Slayer no longer fits this design, so the earlier advice to hunt for one is withdrawn. The brew boiler has become easier: it is a plain hot water vessel with no group neck, so a spare part from almost any stainless dual boiler machine will do.

## 9. Cost

The baseline in bom.csv totals 5,701 USD before contingency: hydraulics 4,051, sensing 370, electrical 535, enclosure 545, hired labour 200. With 15 percent contingency plan on about 6,560. The Raspberry Pi and the ESP32 module are excluded as you asked. The temperature stage accounts for about 590 of the increase over the first draft. For comparison, a used Linea Mini sells for roughly 4,000 to 5,300.

**Profiling hardware, costed.**

- Pressure and flow, Decent style: gear pump about 450, brew pressure transducer about 45. About 495.
- Pressure and flow, Slayer style: stainless vane pump about 280, motor about 150 or more, needle valve 70, two solenoids 130. About 630, and it gives two flow stages only.
- This design's pressure stage: the Decent style hardware plus the bypass branch (solenoid 65, needle valve 70, check valve 35). About 665.
- This design's temperature stage: mix manifold 150, water insert 180, mix valve 70, actuator 60, check valve 35, two fast RTDs 90, hot stub 10. About 595.
- Temperature stage done Decent's way, with a second pump in place of the valve and actuator: about 915.

## 10. Phases

1. Months 1 to 2. Use the Silvia as the test bed. Dev board, one RTD, one SSR, one pressure transducer, the scale platform. Firmware skeleton, link protocol, first page. Spend: about 250.
2. Months 2 to 5. Bench hydraulic rig: gear pump, bypass branch, a needle valve standing in for the puck, then the mix stage with a kettle element in a pot as the hot source. Tune pressure and flow cold, then tune the mix loop and build its calibration table. This phase decides valve or second pump. KiCad board revision A. Spend: about 1,200.
3. Months 4 to 7. Full FreeCAD assembly. Machine the mix manifold, insert and block. Order boilers.
4. Months 7 to 10. Frame, plumbing, wiring. Cold hydrostatic test. Electrical tests: earth bond, GFCI trip, cutoff trip, watchdog drop-out.
5. Months 10 to 13. Thermal tuning, profiles, panels, documentation, template release.

The mix stage adds about a month.

## 11. What will bite

- The mix loop at low flow, and its calibration drifting as the jet scales or the needle wears.
- Flash boiling if the cold branch fails with a 104 C reservoir. Test the abort on the bench.
- Dead volume between the mix point and the screen. Measure it in CAD.
- Thread standards: BSPP against NPT.
- Fittings cost more than any single component except the group. Count them in CAD.
- Low-mineral water against the level probe and against stainless corrosion.
- GFCI nuisance trips from element leakage current as elements age.
- Counterfeit SSRs.
- Load cell drift from heat and from pump, stepper and SSR electrical noise.
- Welded stainless that was not passivated will rust at the welds.
- The custom group's bayonet geometry and gasket seat tolerances.
- The ESP32-C6 pin budget.
- Wi-Fi activity disturbing control timing if the radio and the control loops share a core without care.
- Vacuum on cooldown pulling milk back through the wand if the vacuum breaker sticks.

## 12. Repository layout

- flake.nix: dev shell and template output
- spec/machine.allium: behaviour specification
- docs/design.md, docs/bom.csv, docs/hydraulics.csv
- firmware/: Rust crate for the ESP32-C6
- service/: Haskell service for the Pi
- ui/: PureScript Halogen page
- cad/: FreeCAD files
- pcb/: KiCad project
