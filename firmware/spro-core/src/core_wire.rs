//! The controller's messages as flat runs of numbers.
//!
//! This is the seam between the controller and whatever is driving it.
//! A caller writes numbers into a buffer, names an operation, and reads
//! a status back. The simulator does that through wasm memory. The link
//! from the Pi to the microcontroller will carry the same fields in
//! CBOR frames, and is not written yet.
//!
//! Every field is one `f64`. Enumerations travel as their number, a
//! missing optional value travels as NaN, and times travel as seconds.
//! Nothing read from a buffer is trusted: a number that does not fit
//! its field makes the whole message `Malformed`.

use crate::core_control::Controller;
use crate::core_types::*;

/// The operations a caller can ask for.
pub const OP_INIT: u32 = 0;
pub const OP_SET_PARAMS: u32 = 1;
pub const OP_TICK: u32 = 2;
pub const OP_EVENT: u32 = 3;

/// The events, as the first argument of `OP_EVENT`.
pub const EV_WAKE: u32 = 0;
pub const EV_SLEEP: u32 = 1;
pub const EV_SELECT_PROFILE: u32 = 2;
pub const EV_SHOT: u32 = 3;
pub const EV_ADVANCE: u32 = 4;
pub const EV_STOP: u32 = 5;
pub const EV_CLEAR_FAULTS: u32 = 6;
pub const EV_BLOWDOWN: u32 = 7;
pub const EV_WATER_ACK: u32 = 8;
pub const EV_SENSOR_FAILED: u32 = 9;
pub const EV_FADER_TAKEN: u32 = 10;
pub const EV_FADER_GIVEN: u32 = 11;
pub const EV_FADER_MODE: u32 = 12;

pub const SENSORS_LEN: usize = 11;
pub const CONFIG_LEN: usize = 27;
pub const TUNING_LEN: usize = 16;
pub const PROFILE_HEAD_LEN: usize = 5;
pub const STEP_LEN: usize = 9;
pub const PROFILE_MAX_LEN: usize = PROFILE_HEAD_LEN + MAX_STEPS * STEP_LEN;
pub const STATUS_HEAD_LEN: usize = 38;
pub const FAULT_LEN: usize = 3;
pub const STATUS_LEN: usize = STATUS_HEAD_LEN + MAX_FAULTS * FAULT_LEN;

/// The longest message in either direction.
pub const BUFFER_LEN: usize = 256;

/// Seconds on the wire to milliseconds in the controller.
pub fn to_ms(seconds: f64) -> Ms {
    if seconds <= 0.0 {
        0
    } else {
        (seconds * 1000.0 + 0.5) as Ms
    }
}

fn seconds(ms: Ms) -> f64 {
    ms as f64 / 1000.0
}

/// Reads fields from the front of a buffer, one after another.
pub struct Reader<'a> {
    from: &'a [f64],
    at: usize,
}

type Read<T> = Result<T, Reject>;

impl<'a> Reader<'a> {
    pub fn new(from: &'a [f64]) -> Reader<'a> {
        Reader { from, at: 0 }
    }

    fn next(&mut self) -> Read<f64> {
        let value = *self.from.get(self.at).ok_or(Reject::Malformed)?;
        self.at += 1;
        Ok(value)
    }

    fn real(&mut self) -> Read<Real> {
        let value = self.next()?;
        if value.is_finite() {
            Ok(value as Real)
        } else {
            Err(Reject::Malformed)
        }
    }

    fn optional(&mut self) -> Read<Option<Real>> {
        let value = self.next()?;
        if value.is_nan() {
            Ok(None)
        } else if value.is_finite() {
            Ok(Some(value as Real))
        } else {
            Err(Reject::Malformed)
        }
    }

    /// A whole number from 0 up to and including `most`.
    fn index(&mut self, most: u32) -> Read<u32> {
        let value = self.next()?;
        if value >= 0.0 && value <= most as f64 && value == (value as u32) as f64 {
            Ok(value as u32)
        } else {
            Err(Reject::Malformed)
        }
    }

    fn whole(&mut self) -> Read<i32> {
        let value = self.next()?;
        if (0.0..=1.0e9).contains(&value) && value == (value as i32) as f64 {
            Ok(value as i32)
        } else {
            Err(Reject::Malformed)
        }
    }

    fn flag(&mut self) -> Read<bool> {
        Ok(self.index(1)? == 1)
    }

    pub fn power_mode(&mut self) -> Read<PowerMode> {
        Ok(match self.index(2)? {
            0 => PowerMode::Asleep,
            1 => PowerMode::BrewReady,
            _ => PowerMode::Full,
        })
    }

    pub fn fader_mode(&mut self) -> Read<FaderMode> {
        Ok(match self.index(2)? {
            0 => FaderMode::Pressure,
            1 => FaderMode::Temperature,
            _ => FaderMode::Time,
        })
    }

    pub fn client_kind(&mut self) -> Read<ClientKind> {
        Ok(match self.index(3)? {
            0 => ClientKind::Paddle,
            1 => ClientKind::Remote,
            2 => ClientKind::Service,
            _ => ClientKind::Supervisor,
        })
    }

    pub fn fault_kind(&mut self) -> Read<FaultKind> {
        Ok(match self.index(8)? {
            0 => FaultKind::SensorOpen,
            1 => FaultKind::SensorShort,
            2 => FaultKind::OverTemperature,
            3 => FaultKind::OverPressure,
            4 => FaultKind::FillTimeout,
            5 => FaultKind::NoInletPressure,
            6 => FaultKind::ScaleLost,
            7 => FaultKind::Watchdog,
            _ => FaultKind::MixOverTemperature,
        })
    }

    pub fn sensors(&mut self) -> Read<Sensors> {
        Ok(Sensors {
            inlet_bar: self.real()?,
            inlet_conductivity_us: self.real()?,
            inlet_c: self.real()?,
            brew_bar: self.real()?,
            brew_boiler_c: self.real()?,
            mix_c: self.real()?,
            group_c: self.real()?,
            group_water_c: self.real()?,
            steam_bar: self.real()?,
            steam_level_ok: self.flag()?,
            cup_grams: self.real()?,
        })
    }

    pub fn config(&mut self) -> Read<Config> {
        Ok(Config {
            circuit_budget_watts: self.whole()?,
            brew_element_watts: self.whole()?,
            steam_element_watts: self.whole()?,
            group_heater_watts: self.whole()?,
            reservoir_target_c: self.real()?,
            reservoir_headroom_c: self.real()?,
            steam_target_bar: self.real()?,
            ready_band_c: self.real()?,
            drop_out_band_c: self.real()?,
            steam_ready_band_bar: self.real()?,
            steam_drop_out_band_bar: self.real()?,
            brew_limit_c: self.real()?,
            mix_limit_c: self.real()?,
            group_limit_c: self.real()?,
            steam_limit_bar: self.real()?,
            brew_pressure_limit_bar: self.real()?,
            pace_limit_bar: self.real()?,
            min_inlet_bar: self.real()?,
            max_inlet_conductivity_us: self.real()?,
            fill_timeout_s: self.real()?,
            shot_timeout_s: self.real()?,
            idle_sleep_after_s: self.real()?,
            blowdown_after_fills: self.whole()?,
            manual_min_bar: self.real()?,
            manual_max_bar: self.real()?,
            manual_min_c: self.real()?,
            manual_max_c: self.real()?,
        })
    }

    pub fn tuning(&mut self) -> Read<Tuning> {
        Ok(Tuning {
            pump_max_ml_per_s: self.real()?,
            pump_slip_ml_per_s_bar: self.real()?,
            needle_ml_per_s_bar: self.real()?,
            cold_branch_max_ratio: self.real()?,
            pump_kp: self.real()?,
            pump_ki: self.real()?,
            pump_integral_limit: self.real()?,
            pump_integral_above_bar: self.real()?,
            mix_trim_gain: self.real()?,
            mix_trim_share: self.real()?,
            mix_trim_delay_s: self.real()?,
            mix_min_flow_ml_per_s: self.real()?,
            pace_gain: self.real()?,
            pace_filter_s: self.real()?,
            pace_start_grams: self.real()?,
            heater_turn_s: self.real()?,
        })
    }

    fn step(&mut self) -> Read<Step> {
        Ok(Step {
            mode: match self.index(2)? {
                0 => StepMode::LinePrebrew,
                1 => StepMode::Pressure,
                _ => StepMode::Flow,
            },
            curve: match self.index(3)? {
                0 => Curve::Jump,
                1 => Curve::Linear,
                2 => Curve::Ease,
                _ => Curve::Exponential,
            },
            ramp_s: self.real()?,
            target_c: self.real()?,
            target_bar: self.optional()?,
            target_ml_per_s: self.optional()?,
            max_duration_s: self.real()?,
            exit_above_bar: self.optional()?,
            exit_above_grams: self.optional()?,
        })
    }

    pub fn profile(&mut self) -> Read<Profile> {
        let target_grams = self.real()?;
        let group_base_c = self.real()?;
        let max_target_c = self.real()?;
        let target_seconds = self.optional()?;
        let count = self.index(MAX_STEPS as u32)? as usize;
        let mut steps = [Step::EMPTY; MAX_STEPS];
        for place in steps.iter_mut().take(count) {
            *place = self.step()?;
        }
        Profile::new(
            target_grams,
            group_base_c,
            max_target_c,
            target_seconds,
            &steps[..count],
        )
        .ok_or(Reject::Malformed)
    }

    /// The event numbered `code`, with whatever it carries read from
    /// the buffer.
    pub fn event(&mut self, code: u32) -> Read<Event> {
        Ok(match code {
            EV_WAKE => Event::WakeRequested(self.power_mode()?),
            EV_SLEEP => Event::SleepRequested,
            EV_SELECT_PROFILE => Event::ProfileSelected(self.profile()?),
            EV_SHOT => Event::ShotRequested,
            EV_ADVANCE => Event::StepAdvanceRequested,
            EV_STOP => Event::ShotStopRequested,
            EV_CLEAR_FAULTS => Event::FaultsCleared(self.client_kind()?),
            EV_BLOWDOWN => Event::BlowdownConfirmed,
            EV_WATER_ACK => Event::WaterWarningAcknowledged,
            EV_SENSOR_FAILED => Event::SensorFailed(self.fault_kind()?),
            EV_FADER_TAKEN => Event::FaderTaken(self.real()?),
            EV_FADER_GIVEN => Event::FaderGiven,
            EV_FADER_MODE => Event::FaderModeSelected(self.fader_mode()?),
            _ => return Err(Reject::Malformed),
        })
    }
}

fn optional(value: Option<Real>) -> f64 {
    match value {
        Some(v) => v as f64,
        None => f64::NAN,
    }
}

fn flag(value: bool) -> f64 {
    if value {
        1.0
    } else {
        0.0
    }
}

/// Everything the controller tells the outside about itself, written
/// to the front of `out`. `out` must hold at least `STATUS_LEN` values.
pub fn write_status(c: &Controller, out: &mut [f64]) {
    let a = &c.act;
    let head: [f64; STATUS_HEAD_LEN] = [
        c.requested_mode as u8 as f64,
        c.state as u8 as f64,
        flag(a.heat_enabled),
        flag(a.brew_element_on),
        flag(a.steam_element_on),
        flag(a.group_heater_on),
        a.pump as u8 as f64,
        a.pump_speed as f64,
        a.bypass_valve as u8 as f64,
        a.fill_valve as u8 as f64,
        a.group_valve as u8 as f64,
        a.mix_valve as u8 as f64,
        a.mix_position as f64,
        c.group_target_c as f64,
        optional(c.mix_target_c),
        optional(c.pressure_target_bar),
        optional(c.flow_target_ml_per_s),
        c.pace_trim_bar as f64,
        c.fills_since_blowdown as f64,
        flag(c.blowdown_due),
        flag(c.water_warning),
        flag(c.shot.is_some()),
        c.shot.map_or(0.0, |shot| shot.step_index as f64),
        c.shot.map_or(0.0, |shot| seconds(shot.started_at)),
        c.shot.map_or(0.0, |shot| seconds(shot.step_started_at)),
        c.shots_ended as f64,
        c.last_shot.map_or(0.0, |shot| shot.state as u8 as f64),
        c.last_shot.map_or(0.0, |shot| seconds(shot.started_at)),
        c.last_shot.map_or(0.0, |shot| seconds(shot.ended_at)),
        c.last_shot.map_or(f64::NAN, |shot| optional(shot.final_grams)),
        c.last_shot.map_or(0.0, |shot| shot.step_index as f64),
        c.heater_draw_watts() as f64,
        c.fault_count as f64,
        c.flow_ml_per_s as f64,
        match c.shot {
            Some(Shot {
                manual: Some(Hold::Bar(bar)),
                ..
            }) => bar as f64,
            _ => f64::NAN,
        },
        c.fader as f64,
        c.fader_mode as u8 as f64,
        match c.shot {
            Some(Shot {
                manual: Some(Hold::Celsius(celsius)),
                ..
            }) => celsius as f64,
            _ => f64::NAN,
        },
    ];
    out[..STATUS_HEAD_LEN].copy_from_slice(&head);
    for (i, place) in out[STATUS_HEAD_LEN..STATUS_LEN].chunks_exact_mut(FAULT_LEN).enumerate() {
        match c.faults().get(i) {
            Some(fault) => {
                place[0] = fault.kind as u8 as f64;
                place[1] = seconds(fault.raised_at);
                place[2] = fault.cleared_at.map_or(f64::NAN, seconds);
            }
            None => place.fill(0.0),
        }
    }
}

/// A new controller from a buffer holding a config, a tuning and a
/// profile.
pub fn init(input: &[f64]) -> Result<Controller, Reject> {
    let mut r = Reader::new(input);
    let cfg = r.config()?;
    let tune = r.tuning()?;
    let profile = r.profile()?;
    Ok(Controller::new(cfg, tune, profile))
}

/// One operation on a controller that exists. `a` and `b` are the
/// operation's two plain arguments and `input` holds the rest:
///
/// - `OP_SET_PARAMS`: input is a config and a tuning.
/// - `OP_TICK`: `a` is the cycle time and `b` the time now, both in
///   seconds. Input is the sensors.
/// - `OP_EVENT`: `a` is the event's number and `b` the time now in
///   seconds. Input is the sensors, then whatever the event carries.
pub fn apply(c: &mut Controller, op: u32, a: f64, b: f64, input: &[f64]) -> Result<(), Reject> {
    let mut r = Reader::new(input);
    match op {
        OP_SET_PARAMS => {
            let cfg = r.config()?;
            let tune = r.tuning()?;
            c.cfg = cfg;
            c.tune = tune;
            Ok(())
        }
        OP_TICK => {
            let s = r.sensors()?;
            if !(a > 0.0 && a <= 1.0) {
                return Err(Reject::Malformed);
            }
            c.tick(a as Real, to_ms(b), &s);
            Ok(())
        }
        OP_EVENT => {
            let s = r.sensors()?;
            if !(0.0..=255.0).contains(&a) {
                return Err(Reject::Malformed);
            }
            let event = r.event(a as u32)?;
            c.handle(to_ms(b), &s, event)
        }
        _ => Err(Reject::Malformed),
    }
}
