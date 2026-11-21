//! The controller: the rules of `spec/machine.allium` and the control
//! laws the spec leaves open.
//!
//! `handle` is the event side of the spec. Each case names the rule it
//! implements and returns the unmet `requires` clause when the rule
//! does not apply. `tick` is everything the spec triggers on a
//! condition becoming true or on time passing, plus the pump, mix,
//! pacing and heater laws.

use crate::core_curve::shape;
use crate::core_types::*;

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct RampFrom {
    pub bar: Real,
    pub flow: Real,
    pub temp_c: Real,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Controller {
    /// Limits and setpoints, and calibration and gains. These are the
    /// firmware's stored parameters.
    pub cfg: Config,
    pub tune: Tuning,
    pub requested_mode: PowerMode,
    pub state: MachineState,
    pub last_activity_at: Ms,
    pub act: Actuators,
    pub group_target_c: Real,
    pub mix_target_c: Option<Real>,
    pub pressure_target_bar: Option<Real>,
    pub flow_target_ml_per_s: Option<Real>,
    /// Where each setpoint stood when the present step began.
    pub ramp_from: RampFrom,
    /// How far pacing has moved the pressure setpoint, the smoothed
    /// flow into the cup it works from, and the last scale reading.
    pub pace_trim_bar: Real,
    pub cup_rate_g_per_s: Real,
    pub last_cup_grams: Real,
    pub fill_started_at: Option<Ms>,
    pub fills_since_blowdown: i32,
    pub blowdown_due: bool,
    pub water_warning: bool,
    pub selected: Profile,
    /// At most one shot can be running because there is one slot for it.
    pub shot: Option<Shot>,
    /// How many shots have ended since power-on, and the last of them.
    /// The log of all shots is kept by whoever is listening.
    pub shots_ended: u32,
    pub last_shot: Option<ShotRecord>,
    /// The newest faults, oldest first, and how many of the places are
    /// in use.
    pub faults: [Fault; MAX_FAULTS],
    pub fault_count: usize,
    pub pump_integral: Real,
    pub mix_trim: Real,
    /// Flow into the group as estimated at the last cycle. The machine
    /// has no flow meter, so this is the only flow it can report.
    pub flow_ml_per_s: Real,
    /// Where the fader is along its travel, from 0 to 1, and how fast
    /// the motor is moving it, in travels per second.
    pub fader: Real,
    pub fader_speed: Real,
    /// What the selector knob says the fader's travel stands for.
    pub fader_mode: FaderMode,
}

#[derive(Clone, Copy)]
enum Heater {
    Brew,
    Steam,
    Group,
}

const NO_FAULT: Fault = Fault {
    kind: FaultKind::Watchdog,
    raised_at: 0,
    cleared_at: None,
};

impl Controller {
    /// A sleeping machine with the given parameters and profile.
    pub fn new(cfg: Config, tune: Tuning, profile: Profile) -> Controller {
        Controller {
            cfg,
            tune,
            requested_mode: PowerMode::Asleep,
            state: MachineState::Sleeping,
            last_activity_at: 0,
            act: Actuators::IDLE,
            group_target_c: profile.group_base_c,
            mix_target_c: None,
            pressure_target_bar: None,
            flow_target_ml_per_s: None,
            ramp_from: RampFrom {
                bar: 0.0,
                flow: 0.0,
                temp_c: profile.group_base_c,
            },
            pace_trim_bar: 0.0,
            cup_rate_g_per_s: 0.0,
            last_cup_grams: 0.0,
            fill_started_at: None,
            fills_since_blowdown: 0,
            blowdown_due: false,
            water_warning: false,
            selected: profile,
            shot: None,
            shots_ended: 0,
            last_shot: None,
            faults: [NO_FAULT; MAX_FAULTS],
            fault_count: 0,
            pump_integral: 0.0,
            mix_trim: 0.0,
            flow_ml_per_s: 0.0,
            fader: 0.0,
            fader_speed: 0.0,
            fader_mode: FaderMode::Pressure,
        }
    }

    pub fn faults(&self) -> &[Fault] {
        &self.faults[..self.fault_count]
    }

    // --------------------------------------------------------
    // Derived values of the spec
    // --------------------------------------------------------

    pub fn shot_in_progress(&self) -> bool {
        self.shot.is_some()
    }

    fn awake(&self) -> bool {
        self.state == MachineState::Heating || self.state == MachineState::Ready
    }

    fn inlet_ok(&self, s: &Sensors) -> bool {
        s.inlet_bar >= self.cfg.min_inlet_bar
    }

    fn brew_side_ready(&self, s: &Sensors) -> bool {
        let band = self.cfg.ready_band_c;
        let within = |target: Real, value: Real| value >= target - band && value <= target + band;
        within(self.cfg.reservoir_target_c, s.brew_boiler_c) && within(self.group_target_c, s.group_c)
    }

    /// The condition for becoming ready. The steam side counts as ready
    /// a little below its setpoint, because the element switches at the
    /// setpoint and the pressure hovers just under it.
    pub fn targets_met(&self, s: &Sensors) -> bool {
        self.brew_side_ready(s)
            && (self.requested_mode == PowerMode::BrewReady
                || s.steam_bar >= self.cfg.steam_target_bar - self.cfg.steam_ready_band_bar)
    }

    /// The condition for ceasing to be ready. It uses wider bands than
    /// `targets_met`, so a reading that hovers at the edge of the ready
    /// band does not flip the machine in and out of ready.
    pub fn targets_lost(&self, s: &Sensors) -> bool {
        let band = self.cfg.drop_out_band_c;
        let outside = |target: Real, value: Real| value < target - band || value > target + band;
        outside(self.cfg.reservoir_target_c, s.brew_boiler_c)
            || outside(self.group_target_c, s.group_c)
            || (self.requested_mode == PowerMode::Full
                && s.steam_bar < self.cfg.steam_target_bar - self.cfg.steam_drop_out_band_bar)
    }

    fn needs_fill(&self, s: &Sensors) -> bool {
        !s.steam_level_ok && self.requested_mode == PowerMode::Full
    }

    fn over_limit(&self, s: &Sensors) -> bool {
        s.brew_boiler_c > self.cfg.brew_limit_c
            || s.group_c > self.cfg.group_limit_c
            || s.steam_bar > self.cfg.steam_limit_bar
    }

    fn over_pressure(&self, s: &Sensors) -> bool {
        s.brew_bar > self.cfg.brew_pressure_limit_bar
    }

    /// What the heaters that are on draw between them.
    pub fn heater_draw_watts(&self) -> i32 {
        (if self.act.brew_element_on {
            self.cfg.brew_element_watts
        } else {
            0
        }) + (if self.act.steam_element_on {
            self.cfg.steam_element_watts
        } else {
            0
        }) + (if self.act.group_heater_on {
            self.cfg.group_heater_watts
        } else {
            0
        })
    }

    /// Flow into the group as the firmware estimates it, from pump
    /// speed and slip, or from the needle valve when the bypass is
    /// feeding.
    fn flow_estimate(&self, s: &Sensors) -> Real {
        if self.act.bypass_valve == TwoWay::Open {
            max(0.0, s.inlet_bar - s.brew_bar) * self.tune.needle_ml_per_s_bar
        } else {
            match self.act.pump {
                PumpMode::Stopped => 0.0,
                _ => max(
                    0.0,
                    self.act.pump_speed * self.tune.pump_max_ml_per_s - self.tune.pump_slip_ml_per_s_bar * s.brew_bar,
                ),
            }
        }
    }

    // --------------------------------------------------------
    // Shared effects
    // --------------------------------------------------------

    /// Pump stopped, bypass closed, group vented, mix valve parked.
    fn hydraulics_idle(&mut self) {
        self.act.pump = PumpMode::Stopped;
        self.act.pump_speed = 0.0;
        self.act.bypass_valve = TwoWay::Closed;
        self.act.group_valve = GroupValve::ToDrain;
        self.act.mix_valve = MixValve::Parked;
        self.act.mix_position = 0.0;
    }

    fn clear_targets(&mut self) {
        self.mix_target_c = None;
        self.pressure_target_bar = None;
        self.flow_target_ml_per_s = None;
    }

    fn record(&mut self, shot: Shot, state: ShotState, now: Ms, final_grams: Option<Real>) {
        self.last_shot = Some(ShotRecord {
            state,
            step_index: shot.step_index,
            started_at: shot.started_at,
            ended_at: now,
            final_grams,
        });
        self.shots_ended = self.shots_ended.wrapping_add(1);
        self.shot = None;
    }

    fn remember_fault(&mut self, fault: Fault) {
        if self.fault_count == MAX_FAULTS {
            self.faults.copy_within(1.., 0);
            self.fault_count -= 1;
        }
        self.faults[self.fault_count] = fault;
        self.fault_count += 1;
    }

    /// rule EnterFault
    fn enter_fault(&mut self, now: Ms, kind: FaultKind) {
        if let Some(shot) = self.shot {
            self.record(shot, ShotState::Aborted, now, None);
        }
        self.state = MachineState::Faulted;
        self.act = Actuators::IDLE;
        self.remember_fault(Fault {
            kind,
            raised_at: now,
            cleared_at: None,
        });
        self.clear_targets();
        self.fill_started_at = None;
        self.pump_integral = 0.0;
        self.mix_trim = 0.0;
        self.pace_trim_bar = 0.0;
        self.flow_ml_per_s = 0.0;
    }

    /// rule ApplySleep
    fn apply_sleep(&mut self) {
        self.requested_mode = PowerMode::Asleep;
        self.state = MachineState::Sleeping;
        self.act = Actuators::IDLE;
        self.clear_targets();
        self.fill_started_at = None;
    }

    /// rule EndShot
    fn end_shot(&mut self, now: Ms, s: &Sensors, outcome: ShotState, shot: Shot) {
        self.record(shot, outcome, now, Some(s.cup_grams));
        self.hydraulics_idle();
        self.clear_targets();
        self.last_activity_at = now;
        self.pump_integral = 0.0;
        self.mix_trim = 0.0;
        self.pace_trim_bar = 0.0;
    }

    /// rules EnterLinePrebrew, EnterPressureStep and EnterFlowStep. The
    /// hydraulic configuration changes at once. The setpoints start
    /// from where they stood and are moved by `follow_ramp`.
    fn enter_step(&mut self, s: &Sensors, step: Step) {
        self.ramp_from = RampFrom {
            bar: self.pressure_target_bar.unwrap_or(s.brew_bar),
            flow: self.flow_target_ml_per_s.unwrap_or(self.flow_estimate(s)),
            temp_c: self.mix_target_c.unwrap_or(step.target_c),
        };
        match step.mode {
            StepMode::LinePrebrew => {
                self.act.pump = PumpMode::Stopped;
                self.act.pump_speed = 0.0;
                self.act.bypass_valve = TwoWay::Open;
            }
            StepMode::Pressure => {
                self.act.bypass_valve = TwoWay::Closed;
                self.act.pump = PumpMode::PressureControl;
            }
            StepMode::Flow => {
                self.act.bypass_valve = TwoWay::Closed;
                self.act.pump = PumpMode::FlowControl;
            }
        }
    }

    /// rules SetStepTemperature and RampFinished, and the path between
    /// them: each setpoint travels from its value at step entry to the
    /// step's target along the step's curve, then holds.
    fn follow_ramp(&mut self, now: Ms, shot: Shot) {
        let step = shot.step;
        let fraction = if step.ramp_s <= 0.0 {
            1.0
        } else {
            shape(step.curve, elapsed(shot.step_started_at, now) / step.ramp_s)
        };
        let blend = |from: Real, to: Real| from + (to - from) * fraction;
        self.mix_target_c = Some(blend(self.ramp_from.temp_c, step.target_c));
        self.pressure_target_bar = match step.mode {
            // Pacing moves the setpoint, never past a bar short of the limit.
            StepMode::Pressure => Some(clamp(
                0.5,
                self.cfg.brew_pressure_limit_bar - 1.0,
                blend(self.ramp_from.bar, step.target_bar.unwrap_or(9.0)) + self.pace_trim_bar,
            )),
            _ => None,
        };
        self.flow_target_ml_per_s = match step.mode {
            StepMode::Flow => Some(blend(self.ramp_from.flow, step.target_ml_per_s.unwrap_or(2.0))),
            _ => None,
        };
    }

    /// rules AdvanceStep and LastStepFinished
    fn finish_step(&mut self, now: Ms, s: &Sensors, shot: Shot) {
        match self.selected.step(shot.step_index + 1) {
            Some(next) => {
                let advanced = Shot {
                    step_index: shot.step_index + 1,
                    step: next,
                    started_at: shot.started_at,
                    step_started_at: now,
                    // A temperature held by hand stays held from step
                    // to step. A pressure does not.
                    manual: match shot.manual {
                        Some(Hold::Celsius(c)) => Some(Hold::Celsius(c)),
                        _ => None,
                    },
                };
                self.shot = Some(advanced);
                self.enter_step(s, next);
                self.follow_ramp(now, advanced);
            }
            None => self.end_shot(now, s, ShotState::StoppedByTime, shot),
        }
    }

    // --------------------------------------------------------
    // Events
    // --------------------------------------------------------

    /// Apply one event. Nothing changes when the event is refused.
    pub fn handle(&mut self, now: Ms, s: &Sensors, event: Event) -> Result<(), Reject> {
        match event {
            // rules Wake and ChangeMode
            Event::WakeRequested(mode) => {
                if mode == PowerMode::Asleep {
                    return Err(Reject::WakeNeedsMode);
                }
                if self.state == MachineState::Faulted {
                    return Err(Reject::Faulted);
                }
                if self.state == MachineState::Sleeping {
                    self.requested_mode = mode;
                    self.state = MachineState::Heating;
                    self.act.heat_enabled = true;
                    self.last_activity_at = now;
                } else {
                    self.requested_mode = mode;
                    self.last_activity_at = now;
                    if mode != PowerMode::Full {
                        self.act.steam_element_on = false;
                        self.act.fill_valve = TwoWay::Closed;
                        self.fill_started_at = None;
                    }
                }
                Ok(())
            }

            // rule Sleep
            Event::SleepRequested => {
                if !self.awake() {
                    return Err(Reject::NotAwake);
                }
                if self.shot_in_progress() {
                    return Err(Reject::ShotRunning);
                }
                self.apply_sleep();
                Ok(())
            }

            // rule SelectProfile
            Event::ProfileSelected(profile) => {
                if self.state == MachineState::Faulted {
                    return Err(Reject::Faulted);
                }
                if self.shot_in_progress() {
                    return Err(Reject::ShotRunning);
                }
                self.selected = profile;
                self.group_target_c = profile.group_base_c;
                self.last_activity_at = now;
                Ok(())
            }

            // rule StartShot
            Event::ShotRequested => {
                if self.state != MachineState::Ready {
                    return Err(Reject::NotReady);
                }
                if self.targets_lost(s) {
                    return Err(Reject::TemperaturesDrifted);
                }
                if self.group_target_c != self.selected.group_base_c {
                    return Err(Reject::GroupSetForAnother);
                }
                if s.brew_boiler_c < self.selected.max_target_c + self.cfg.reservoir_headroom_c {
                    return Err(Reject::ReservoirTooCool);
                }
                if self.selected.max_target_c > self.cfg.mix_limit_c {
                    return Err(Reject::ProfileAboveMixLimit);
                }
                if self.shot_in_progress() {
                    return Err(Reject::ShotAlreadyRunning);
                }
                if self.act.fill_valve == TwoWay::Open {
                    return Err(Reject::SteamBoilerFilling);
                }
                if !self.inlet_ok(s) {
                    return Err(Reject::NoInletPressure);
                }
                let first = self.selected.first();
                let shot = Shot {
                    step_index: 0,
                    step: first,
                    started_at: now,
                    step_started_at: now,
                    manual: None,
                };
                self.shot = Some(shot);
                self.last_activity_at = now;
                self.mix_target_c = None;
                self.pace_trim_bar = 0.0;
                self.cup_rate_g_per_s = 0.0;
                self.last_cup_grams = 0.0;
                self.act.group_valve = GroupValve::ToGroup;
                self.act.mix_valve = MixValve::Regulating;
                self.enter_step(s, first);
                self.follow_ramp(now, shot);
                Ok(())
            }

            // rule AdvanceByUser
            Event::StepAdvanceRequested => match self.shot {
                None => Err(Reject::NoShotRunning),
                Some(shot) => {
                    self.finish_step(now, s, shot);
                    Ok(())
                }
            },

            // rule StopByUser
            Event::ShotStopRequested => match self.shot {
                None => Err(Reject::NoShotRunning),
                Some(shot) => {
                    self.end_shot(now, s, ShotState::StoppedByUser, shot);
                    Ok(())
                }
            },

            // rule TakeFader
            Event::FaderTaken(reading) => match (self.shot, self.fader_mode) {
                (_, FaderMode::Time) => Err(Reject::FaderShowsTime),
                (None, _) => Err(Reject::NoShotRunning),
                (Some(shot), FaderMode::Pressure) => {
                    let level = self.manual_level(reading);
                    self.shot = Some(Shot {
                        manual: Some(Hold::Bar(level)),
                        ..shot
                    });
                    // Manual mode is pressure on the pump, whatever the
                    // step was doing.
                    self.act.bypass_valve = TwoWay::Closed;
                    self.act.pump = PumpMode::PressureControl;
                    self.pace_trim_bar = 0.0;
                    self.flow_target_ml_per_s = None;
                    self.pressure_target_bar = Some(level);
                    Ok(())
                }
                (Some(shot), FaderMode::Temperature) => {
                    let level = self.manual_water(reading);
                    self.shot = Some(Shot {
                        manual: Some(Hold::Celsius(level)),
                        ..shot
                    });
                    self.mix_target_c = Some(level);
                    Ok(())
                }
            },

            // rule GiveFaderBack
            Event::FaderGiven => match self.shot {
                None => Err(Reject::NoShotRunning),
                Some(shot) => {
                    self.give_back(now, s, shot);
                    Ok(())
                }
            },

            // rule TurnSelector
            Event::FaderModeSelected(mode) => {
                // Whatever the hand was holding on the old scale is
                // let go, so the fader never carries a reading from one
                // scale onto another.
                if mode != self.fader_mode {
                    if let Some(shot) = self.shot {
                        self.give_back(now, s, shot);
                    }
                    self.fader_mode = mode;
                }
                Ok(())
            }

            // rule ClearFaults
            Event::FaultsCleared(client) => {
                if self.state != MachineState::Faulted {
                    return Err(Reject::NotFaulted);
                }
                if client != ClientKind::Service {
                    return Err(Reject::OnlyServiceClears);
                }
                if self.over_limit(s) {
                    return Err(Reject::StillOverLimit);
                }
                if self.over_pressure(s) {
                    return Err(Reject::StillOverPressure);
                }
                for fault in self.faults[..self.fault_count].iter_mut() {
                    if fault.cleared_at.is_none() {
                        fault.cleared_at = Some(now);
                    }
                }
                self.requested_mode = PowerMode::Asleep;
                self.state = MachineState::Sleeping;
                Ok(())
            }

            // rule BlowdownDone
            Event::BlowdownConfirmed => {
                if self.state != MachineState::Sleeping {
                    return Err(Reject::BlowdownNeedsSleep);
                }
                self.fills_since_blowdown = 0;
                self.blowdown_due = false;
                Ok(())
            }

            // rule WaterWarningCleared
            Event::WaterWarningAcknowledged => {
                if s.inlet_conductivity_us > self.cfg.max_inlet_conductivity_us {
                    return Err(Reject::WaterStillOutOfRange);
                }
                self.water_warning = false;
                Ok(())
            }

            // rule SensorFault
            Event::SensorFailed(kind) => {
                if self.state == MachineState::Faulted {
                    return Err(Reject::AlreadyFaulted);
                }
                match kind {
                    FaultKind::SensorOpen | FaultKind::SensorShort | FaultKind::ScaleLost | FaultKind::Watchdog => {
                        self.enter_fault(now, kind);
                        Ok(())
                    }
                    _ => Err(Reject::NotASensorFailure),
                }
            }
        }
    }

    // --------------------------------------------------------
    // Periodic work
    // --------------------------------------------------------

    /// One controller cycle. `dt` is the cycle time in seconds.
    pub fn tick(&mut self, dt: Real, now: Ms, s: &Sensors) {
        if self.state == MachineState::Faulted {
            return;
        }
        if let Some(kind) = self.detect_fault(now, s) {
            self.enter_fault(now, kind);
            return;
        }
        self.track_readiness(now, s);
        self.idle_sleep(now);
        self.water_quality(s);
        self.steam_fill(now, s);
        self.run_shot(dt, now, s);
        self.allocate_heaters(now, s);
        self.flow_ml_per_s = self.flow_estimate(s);
        self.move_fader(dt, now, s);
    }

    /// The profile takes back whatever the hand was holding. After a
    /// pressure the step begins again from where the hand left it and
    /// ramps to its own target. After a temperature the step was never
    /// stopped, and the water goes back to what the step asks for.
    fn give_back(&mut self, now: Ms, s: &Sensors, shot: Shot) {
        match shot.manual {
            Some(Hold::Bar(_)) => {
                let resumed = Shot {
                    manual: None,
                    step_started_at: now,
                    ..shot
                };
                self.shot = Some(resumed);
                self.enter_step(s, resumed.step);
                self.follow_ramp(now, resumed);
            }
            Some(Hold::Celsius(_)) => {
                let resumed = Shot { manual: None, ..shot };
                self.shot = Some(resumed);
                self.follow_ramp(now, resumed);
            }
            None => {}
        }
    }

    /// A pressure asked for by hand, held between the manual limits and
    /// never past a bar short of the pressure limit.
    fn manual_level(&self, bar: Real) -> Real {
        let most = min(self.cfg.manual_max_bar, self.cfg.brew_pressure_limit_bar - 1.0);
        clamp(min(self.cfg.manual_min_bar, most), most, bar)
    }

    /// A water temperature asked for by hand, held between the manual
    /// limits and never past a degree short of the mix limit.
    fn manual_water(&self, c: Real) -> Real {
        let most = min(self.cfg.manual_max_c, self.cfg.mix_limit_c - 1.0);
        clamp(min(self.cfg.manual_min_c, most), most, c)
    }

    /// The fader shows what the selector has chosen. On pressure, the
    /// motor carries it to the pressure setpoint in a pressure step and
    /// to the measured pressure in the steps that have none. On
    /// temperature, to the water temperature asked for. On time, to how
    /// much of the shot's target time has passed, or of the shot
    /// timeout when the profile has no target time. With no shot
    /// running it goes back to rest. The motion is eased, so a setpoint
    /// that jumps moves the fader smoothly. While the hand holds a
    /// value the motor is off and the fader is wherever the hand has
    /// put it.
    fn move_fader(&mut self, dt: Real, now: Ms, s: &Sensors) {
        let along = |low: Real, high: Real, value: Real| clamp(0.0, 1.0, (value - low) / (high - low));
        let held = match self.shot {
            Some(Shot {
                manual: Some(Hold::Bar(bar)),
                ..
            }) => Some(along(0.0, FADER_FULL_BAR, self.manual_level(bar))),
            Some(Shot {
                manual: Some(Hold::Celsius(c)),
                ..
            }) => Some(along(FADER_LOW_C, FADER_HIGH_C, self.manual_water(c))),
            _ => None,
        };
        match held {
            Some(place) => {
                self.fader = place;
                self.fader_speed = 0.0;
            }
            None => {
                let wanted = match (self.shot, self.fader_mode) {
                    (None, _) => 0.0,
                    (Some(_), FaderMode::Pressure) => {
                        along(0.0, FADER_FULL_BAR, self.pressure_target_bar.unwrap_or(s.brew_bar))
                    }
                    (Some(shot), FaderMode::Temperature) => along(
                        FADER_LOW_C,
                        FADER_HIGH_C,
                        self.mix_target_c.unwrap_or(shot.step.target_c),
                    ),
                    (Some(shot), FaderMode::Time) => {
                        let whole = self.selected.target_seconds.unwrap_or(self.cfg.shot_timeout_s);
                        along(0.0, max(1.0, whole), elapsed(shot.started_at, now))
                    }
                };
                // The place moves at the speed it had, and then the
                // speed changes, so the fader leaves rest at no speed.
                let pull = FADER_STIFFNESS * FADER_STIFFNESS * (wanted - self.fader)
                    - 2.0 * FADER_STIFFNESS * self.fader_speed;
                self.fader = clamp(0.0, 1.0, self.fader + self.fader_speed * dt);
                self.fader_speed += pull * dt;
            }
        }
    }

    /// rules OverLimit, OverPressure, MixTooHot, InletLostDuringShot and
    /// FillTimedOut
    fn detect_fault(&self, now: Ms, s: &Sensors) -> Option<FaultKind> {
        if self.over_limit(s) {
            return Some(FaultKind::OverTemperature);
        }
        if self.over_pressure(s) {
            return Some(FaultKind::OverPressure);
        }
        if self.shot_in_progress() && s.mix_c > self.cfg.mix_limit_c {
            return Some(FaultKind::MixOverTemperature);
        }
        if self.shot_in_progress() && !self.inlet_ok(s) {
            return Some(FaultKind::NoInletPressure);
        }
        match self.fill_started_at {
            Some(started)
                if self.act.fill_valve == TwoWay::Open && elapsed(started, now) >= self.cfg.fill_timeout_s =>
            {
                Some(FaultKind::FillTimeout)
            }
            _ => None,
        }
    }

    /// rules BecomeReady and FallBackToHeating. Becoming ready restarts
    /// the idle clock.
    fn track_readiness(&mut self, now: Ms, s: &Sensors) {
        if !self.awake() || self.shot_in_progress() {
            return;
        }
        if self.state == MachineState::Ready {
            if self.targets_lost(s) {
                self.state = MachineState::Heating;
            }
        } else if self.targets_met(s) {
            self.state = MachineState::Ready;
            self.last_activity_at = now;
        }
    }

    /// rule IdleSleep
    fn idle_sleep(&mut self, now: Ms) {
        if self.awake()
            && !self.shot_in_progress()
            && elapsed(self.last_activity_at, now) >= self.cfg.idle_sleep_after_s
        {
            self.apply_sleep();
        }
    }

    /// rules HardWaterSeen and BlowdownBecomesDue
    fn water_quality(&mut self, s: &Sensors) {
        self.water_warning = self.water_warning || s.inlet_conductivity_us > self.cfg.max_inlet_conductivity_us;
        self.blowdown_due = self.blowdown_due || self.fills_since_blowdown >= self.cfg.blowdown_after_fills;
    }

    /// rules FillNeeded, StartFill, RetryFillWhenInletReturns and FinishFill
    fn steam_fill(&mut self, now: Ms, s: &Sensors) {
        if self.act.fill_valve == TwoWay::Open && s.steam_level_ok {
            self.act.fill_valve = TwoWay::Closed;
            self.fill_started_at = None;
            self.fills_since_blowdown += 1;
        } else if self.act.fill_valve == TwoWay::Closed
            && self.awake()
            && self.needs_fill(s)
            && self.inlet_ok(s)
            && !self.shot_in_progress()
        {
            self.act.fill_valve = TwoWay::Open;
            self.act.steam_element_on = false;
            self.fill_started_at = Some(now);
        }
    }

    /// Step exits, shot stops and the pump and mix control laws.
    fn run_shot(&mut self, dt: Real, now: Ms, s: &Sensors) {
        let shot = match self.shot {
            Some(shot) => shot,
            None => return,
        };
        let reached = |limit: Option<Real>, value: Real| match limit {
            Some(l) => value >= l,
            None => false,
        };
        // rule StopOnWeight
        if s.cup_grams >= self.selected.target_grams {
            self.end_shot(now, s, ShotState::StoppedByWeight, shot);
        // rule StopOnTimeout
        } else if elapsed(shot.started_at, now) >= self.cfg.shot_timeout_s {
            self.end_shot(now, s, ShotState::StoppedByTime, shot);
        } else if let Some(Hold::Bar(bar)) = shot.manual {
            // Manual mode: the fader is the pressure setpoint, held
            // between the manual limits. The steps stand still, and the
            // water stays at the temperature of the step the shot was in.
            self.pressure_target_bar = Some(self.manual_level(bar));
            self.pump_law(dt, s, StepMode::Pressure);
            self.mix_law(dt, now, s, shot);
        // rules StepExitsOnTime, StepExitsOnPressure and StepExitsOnWeight
        } else if elapsed(shot.step_started_at, now) >= shot.step.max_duration_s
            || reached(shot.step.exit_above_bar, s.brew_bar)
            || reached(shot.step.exit_above_grams, s.cup_grams)
        {
            self.finish_step(now, s, shot);
        } else {
            self.pace_law(dt, now, s, shot);
            self.follow_ramp(now, shot);
            // A water temperature held by hand stands in for the step's.
            if let Some(Hold::Celsius(c)) = shot.manual {
                self.mix_target_c = Some(self.manual_water(c));
            }
            self.pump_law(dt, s, shot.step.mode);
            self.mix_law(dt, now, s, shot);
        }
    }

    /// rule PaceShot. With a target time on the profile, the machine
    /// works out the flow into the cup that would land the target
    /// weight on time, compares it with the flow the scale shows, and
    /// moves the pressure setpoint to close the gap. It acts only in
    /// pressure steps, only once coffee is reaching the cup, and only
    /// within the pacing limit. A slow puck gets more pressure and a
    /// fast one gets less.
    fn pace_law(&mut self, dt: Real, now: Ms, s: &Sensors, shot: Shot) {
        let raw = (s.cup_grams - self.last_cup_grams) / dt;
        let rate =
            self.cup_rate_g_per_s + (raw - self.cup_rate_g_per_s) * min(1.0, dt / max(0.05, self.tune.pace_filter_s));
        self.cup_rate_g_per_s = rate;
        self.last_cup_grams = s.cup_grams;
        let limit = self.cfg.pace_limit_bar;
        let paced = match self.selected.target_seconds {
            Some(seconds) if shot.step.mode == StepMode::Pressure && s.cup_grams >= self.tune.pace_start_grams => {
                Some(seconds)
            }
            _ => None,
        };
        match paced {
            Some(seconds) => {
                let grams_left = max(0.0, self.selected.target_grams - s.cup_grams);
                let seconds_left = seconds - elapsed(shot.started_at, now);
                // With time in hand, aim to land the rest of the weight
                // on time. In the last 3 s the wanted flow swings wildly
                // with every gram, so the trim holds. Past the target
                // time the shot is late, and the pressure keeps rising
                // towards the limit until it ends.
                let wanted = if seconds_left >= 3.0 {
                    Some(grams_left / seconds_left)
                } else if seconds_left < 0.0 {
                    Some(rate + 0.5)
                } else {
                    None
                };
                if let Some(wanted) = wanted {
                    self.pace_trim_bar = clamp(
                        -limit,
                        limit,
                        self.pace_trim_bar + self.tune.pace_gain * (wanted - rate) * dt,
                    );
                }
            }
            None => self.pace_trim_bar = clamp(-limit, limit, self.pace_trim_bar),
        }
    }

    /// Pressure steps: proportional plus integral on brew pressure,
    /// with the integral frozen until pressure starts to build so
    /// filling the dry puck does not wind it up. Flow steps: pump speed
    /// from the target flow plus the slip expected at the present
    /// pressure.
    fn pump_law(&mut self, dt: Real, s: &Sensors, mode: StepMode) {
        match mode {
            StepMode::LinePrebrew => {}
            StepMode::Pressure => {
                let err = self.pressure_target_bar.unwrap_or(9.0) - s.brew_bar;
                if s.brew_bar > self.tune.pump_integral_above_bar {
                    self.pump_integral = clamp(
                        0.0,
                        self.tune.pump_integral_limit,
                        self.pump_integral + self.tune.pump_ki * err * dt,
                    );
                }
                self.act.pump_speed = clamp(0.0, 1.0, self.tune.pump_kp * err + self.pump_integral);
            }
            StepMode::Flow => {
                let target = self.flow_target_ml_per_s.unwrap_or(2.0);
                self.act.pump_speed = clamp(
                    0.0,
                    1.0,
                    (target + self.tune.pump_slip_ml_per_s_bar * s.brew_bar) / self.tune.pump_max_ml_per_s,
                );
            }
        }
    }

    /// Feed-forward valve position from the three temperatures, trimmed
    /// by the mix sensor. The trim is slow and bounded to a share of
    /// the feed-forward position, so a stale sensor reading cannot
    /// close the cold branch. It freezes for the first moments of a
    /// shot, while the sensor still reads the water that sat in the
    /// manifold, and below a flow threshold, where the sensor sits in
    /// nearly still water.
    fn mix_law(&mut self, dt: Real, now: Ms, s: &Sensors, shot: Shot) {
        let target = self.mix_target_c.unwrap_or(shot.step.target_c);
        let span = s.brew_boiler_c - s.inlet_c;
        let cold_fraction = if span <= 1.0 {
            0.0
        } else {
            clamp(0.0, 0.66, (s.brew_boiler_c - target) / span)
        };
        let feed_forward = cold_fraction / (self.tune.cold_branch_max_ratio * (1.0 - cold_fraction));
        let trim_limit = self.tune.mix_trim_share * feed_forward + 0.02;
        if self.flow_estimate(s) > self.tune.mix_min_flow_ml_per_s
            && elapsed(shot.started_at, now) > self.tune.mix_trim_delay_s
        {
            self.mix_trim = clamp(
                -trim_limit,
                trim_limit,
                self.mix_trim + self.tune.mix_trim_gain * (s.mix_c - target) * dt,
            );
        }
        self.act.mix_position = clamp(0.0, 1.0, feed_forward + self.mix_trim);
    }

    /// The spec bounds the heater total with the CircuitBudget
    /// invariant and leaves the allocation open. This one gives the
    /// brew reservoir first call during a shot and otherwise alternates
    /// turns between the reservoir and the steam boiler.
    fn allocate_heaters(&mut self, now: Ms, s: &Sensors) {
        if !self.awake() {
            return;
        }
        let turn_ms = (max(0.1, self.tune.heater_turn_s) * 1000.0) as Ms;
        let steam_turn = (now / turn_ms).is_multiple_of(2);
        let order = if self.shot_in_progress() {
            [Heater::Brew, Heater::Group, Heater::Steam]
        } else if steam_turn {
            [Heater::Group, Heater::Steam, Heater::Brew]
        } else {
            [Heater::Group, Heater::Brew, Heater::Steam]
        };
        let steam_may = self.requested_mode == PowerMode::Full
            && s.steam_level_ok
            && self.act.fill_valve == TwoWay::Closed
            && s.steam_bar < self.cfg.steam_target_bar;
        self.act.brew_element_on = false;
        self.act.steam_element_on = false;
        self.act.group_heater_on = false;
        let mut used = 0;
        for heater in order {
            let (wants, watts) = match heater {
                Heater::Brew => (
                    s.brew_boiler_c < self.cfg.reservoir_target_c,
                    self.cfg.brew_element_watts,
                ),
                Heater::Group => (s.group_c < self.group_target_c, self.cfg.group_heater_watts),
                Heater::Steam => (steam_may, self.cfg.steam_element_watts),
            };
            if wants && used + watts <= self.cfg.circuit_budget_watts {
                used += watts;
                match heater {
                    Heater::Brew => self.act.brew_element_on = true,
                    Heater::Group => self.act.group_heater_on = true,
                    Heater::Steam => self.act.steam_element_on = true,
                }
            }
        }
    }
}
