//! Host tests of the controller core. The simulator's scenarios run
//! the same code against a simulated machine and go much further. These
//! check the rules that need no machine: refusals, the shot sequence,
//! faults, the heater budget and the message format.

use crate::core_control::Controller;
use crate::core_curve::shape;
use crate::core_types::*;
use crate::core_wire::{self, Reader};

fn config() -> Config {
    Config {
        circuit_budget_watts: 1440,
        brew_element_watts: 600,
        steam_element_watts: 1000,
        group_heater_watts: 150,
        reservoir_target_c: 104.0,
        reservoir_headroom_c: 6.0,
        steam_target_bar: 1.8,
        ready_band_c: 0.3,
        drop_out_band_c: 0.8,
        steam_ready_band_bar: 0.15,
        steam_drop_out_band_bar: 0.3,
        brew_limit_c: 118.0,
        mix_limit_c: 99.0,
        group_limit_c: 110.0,
        steam_limit_bar: 2.3,
        brew_pressure_limit_bar: 11.0,
        pace_limit_bar: 3.0,
        min_inlet_bar: 1.5,
        max_inlet_conductivity_us: 250.0,
        fill_timeout_s: 90.0,
        shot_timeout_s: 120.0,
        idle_sleep_after_s: 2700.0,
        blowdown_after_fills: 200,
    }
}

fn tuning() -> Tuning {
    Tuning {
        pump_max_ml_per_s: 10.0,
        pump_slip_ml_per_s_bar: 0.15,
        needle_ml_per_s_bar: 0.6,
        cold_branch_max_ratio: 2.0,
        pump_kp: 0.06,
        pump_ki: 0.05,
        pump_integral_limit: 0.6,
        pump_integral_above_bar: 1.0,
        mix_trim_gain: 0.004,
        mix_trim_share: 0.5,
        mix_trim_delay_s: 1.5,
        mix_min_flow_ml_per_s: 0.3,
        pace_gain: 0.6,
        pace_filter_s: 1.0,
        pace_start_grams: 6.0,
        heater_turn_s: 2.0,
    }
}

fn wet() -> Step {
    Step {
        mode: StepMode::Flow,
        curve: Curve::Jump,
        ramp_s: 0.0,
        target_c: 93.0,
        target_bar: None,
        target_ml_per_s: Some(2.0),
        max_duration_s: 15.0,
        exit_above_bar: Some(2.0),
        exit_above_grams: None,
    }
}

fn hold() -> Step {
    Step {
        mode: StepMode::Pressure,
        curve: Curve::Linear,
        ramp_s: 4.0,
        target_c: 93.0,
        target_bar: Some(9.0),
        target_ml_per_s: None,
        max_duration_s: 60.0,
        exit_above_bar: None,
        exit_above_grams: None,
    }
}

fn classic() -> Profile {
    Profile::new(36.0, 92.0, 93.0, None, &[wet(), hold()]).unwrap()
}

/// A machine at temperature with water at the inlet.
fn warm() -> Sensors {
    Sensors {
        inlet_bar: 3.0,
        inlet_conductivity_us: 90.0,
        inlet_c: 15.0,
        brew_bar: 0.0,
        brew_boiler_c: 104.0,
        mix_c: 92.0,
        group_c: 92.0,
        group_water_c: 92.0,
        steam_bar: 1.8,
        steam_level_ok: true,
        cup_grams: 0.0,
    }
}

const DT: Real = 0.025;

/// A controller that has been woken and has seen warm sensors once.
fn ready() -> Controller {
    let mut c = Controller::new(config(), tuning(), classic());
    c.handle(0, &warm(), Event::WakeRequested(PowerMode::Full)).unwrap();
    c.tick(DT, 25, &warm());
    assert_eq!(c.state, MachineState::Ready);
    c
}

fn hydraulics_idle(c: &Controller) -> bool {
    c.act.pump == PumpMode::Stopped
        && c.act.bypass_valve == TwoWay::Closed
        && c.act.group_valve == GroupValve::ToDrain
        && c.act.mix_valve == MixValve::Parked
}

#[test]
fn every_curve_starts_at_nothing_and_ends_at_the_target() {
    for curve in [Curve::Linear, Curve::Ease, Curve::Exponential] {
        assert_eq!(shape(curve, 0.0), 0.0);
        assert_eq!(shape(curve, 1.0), 1.0);
        assert_eq!(shape(curve, 3.0), 1.0);
        let mut last = 0.0;
        for i in 0..=100 {
            let y = shape(curve, i as Real / 100.0);
            assert!(y >= last && y <= 1.0, "{curve:?} leaves its range at {i}");
            last = y;
        }
    }
    assert_eq!(shape(Curve::Jump, 0.0), 1.0);
    assert_eq!(shape(Curve::Linear, 0.5), 0.5);
}

#[test]
fn a_sleeping_machine_refuses_a_shot_and_changes_nothing() {
    let mut c = Controller::new(config(), tuning(), classic());
    let before = c;
    assert_eq!(c.handle(0, &warm(), Event::ShotRequested), Err(Reject::NotReady));
    assert_eq!(c.handle(0, &warm(), Event::SleepRequested), Err(Reject::NotAwake));
    assert_eq!(
        c.handle(0, &warm(), Event::WakeRequested(PowerMode::Asleep)),
        Err(Reject::WakeNeedsMode)
    );
    assert_eq!(c, before);
    assert_eq!(c.act, Actuators::IDLE);
}

#[test]
fn waking_heats_and_warm_sensors_make_it_ready() {
    let mut c = Controller::new(config(), tuning(), classic());
    let mut cold = warm();
    cold.brew_boiler_c = 22.0;
    cold.group_c = 22.0;
    cold.steam_bar = 0.0;
    c.handle(0, &cold, Event::WakeRequested(PowerMode::Full)).unwrap();
    c.tick(DT, 25, &cold);
    assert_eq!(c.state, MachineState::Heating);
    assert!(c.act.heat_enabled);
    assert!(c.heater_draw_watts() > 0);
    c.tick(DT, 50, &warm());
    assert_eq!(c.state, MachineState::Ready);
}

#[test]
fn the_heaters_never_draw_more_than_the_budget() {
    let mut c = Controller::new(config(), tuning(), classic());
    let mut cold = warm();
    cold.brew_boiler_c = 22.0;
    cold.group_c = 22.0;
    cold.steam_bar = 0.0;
    c.handle(0, &cold, Event::WakeRequested(PowerMode::Full)).unwrap();
    let mut brew_had_a_turn = false;
    let mut steam_had_a_turn = false;
    for i in 1..=400u64 {
        c.tick(DT, i * 25, &cold);
        assert!(c.heater_draw_watts() <= c.cfg.circuit_budget_watts);
        brew_had_a_turn |= c.act.brew_element_on;
        steam_had_a_turn |= c.act.steam_element_on;
    }
    assert!(brew_had_a_turn && steam_had_a_turn);
}

#[test]
fn a_shot_runs_its_steps_and_stops_on_weight() {
    let mut c = ready();
    let mut s = warm();
    c.handle(1000, &s, Event::ShotRequested).unwrap();
    let shot = c.shot.unwrap();
    assert_eq!(shot.step_index, 0);
    assert_eq!(c.act.pump, PumpMode::FlowControl);
    assert_eq!(c.act.group_valve, GroupValve::ToGroup);
    assert_eq!(c.act.mix_valve, MixValve::Regulating);
    assert_eq!(
        c.handle(1000, &s, Event::ShotRequested),
        Err(Reject::ShotAlreadyRunning)
    );
    assert_eq!(
        c.handle(1000, &s, Event::ProfileSelected(classic())),
        Err(Reject::ShotRunning)
    );

    // Pressure builds: the wetting step exits and the pressure step ramps.
    s.brew_bar = 2.5;
    c.tick(DT, 1025, &s);
    assert_eq!(c.shot.unwrap().step_index, 1);
    assert_eq!(c.act.pump, PumpMode::PressureControl);
    let early = c.pressure_target_bar.unwrap();
    c.tick(DT, 3025, &s);
    let halfway = c.pressure_target_bar.unwrap();
    c.tick(DT, 5025, &s);
    let late = c.pressure_target_bar.unwrap();
    assert!(
        (early - 2.5).abs() < 0.01,
        "the ramp starts from the pressure in the group, got {early}"
    );
    assert!(
        (halfway - 5.75).abs() < 0.01,
        "halfway at half the ramp time, got {halfway}"
    );
    assert_eq!(late, 9.0);
    assert!(late <= c.cfg.brew_pressure_limit_bar - 1.0);

    s.cup_grams = 36.0;
    c.tick(DT, 5050, &s);
    assert!(c.shot.is_none());
    let record = c.last_shot.unwrap();
    assert_eq!(record.state, ShotState::StoppedByWeight);
    assert_eq!(record.final_grams, Some(36.0));
    assert_eq!((record.started_at, record.ended_at), (1000, 5050));
    assert_eq!(c.shots_ended, 1);
    assert!(hydraulics_idle(&c));
    assert_eq!(c.pressure_target_bar, None);
    assert_eq!(c.pace_trim_bar, 0.0);
}

#[test]
fn a_line_prebrew_step_opens_the_bypass_and_stops_the_pump() {
    let mut soak = wet();
    soak.mode = StepMode::LinePrebrew;
    soak.target_ml_per_s = None;
    soak.exit_above_bar = None;
    soak.max_duration_s = 6.0;
    let profile = Profile::new(36.0, 92.0, 93.0, None, &[soak, hold()]).unwrap();
    let mut c = ready();
    c.handle(100, &warm(), Event::ProfileSelected(profile)).unwrap();
    c.tick(DT, 125, &warm());
    c.handle(1000, &warm(), Event::ShotRequested).unwrap();
    assert_eq!(c.act.bypass_valve, TwoWay::Open);
    assert_eq!(c.act.pump, PumpMode::Stopped);
    c.tick(DT, 6975, &warm());
    assert_eq!(c.shot.unwrap().step_index, 0);
    c.tick(DT, 7000, &warm());
    assert_eq!(c.shot.unwrap().step_index, 1);
    assert_eq!(c.act.bypass_valve, TwoWay::Closed);
    assert_eq!(c.act.pump, PumpMode::PressureControl);
}

#[test]
fn pacing_stays_within_its_limit_and_only_acts_when_asked() {
    let paced = Profile::new(36.0, 92.0, 93.0, Some(30.0), &[hold()]).unwrap();
    let mut c = ready();
    c.handle(100, &warm(), Event::ProfileSelected(paced)).unwrap();
    c.tick(DT, 125, &warm());
    let mut s = warm();
    c.handle(1000, &s, Event::ShotRequested).unwrap();
    s.brew_bar = 8.0;
    // A cup that fills far too slowly: pacing adds pressure up to its limit.
    for i in 1..=1200u64 {
        s.cup_grams = 6.0 + i as Real * 0.002;
        c.tick(DT, 1000 + i * 25, &s);
        assert!(c.pace_trim_bar.abs() <= c.cfg.pace_limit_bar);
        if let Some(bar) = c.pressure_target_bar {
            assert!(bar <= c.cfg.brew_pressure_limit_bar - 1.0);
        }
    }
    assert!(
        c.pace_trim_bar > 2.9,
        "a slow cup gets more pressure, got {}",
        c.pace_trim_bar
    );

    let mut free = ready();
    free.handle(1000, &warm(), Event::ShotRequested).unwrap();
    for i in 1..=400u64 {
        free.tick(DT, 1000 + i * 25, &s);
    }
    assert_eq!(free.pace_trim_bar, 0.0);
}

#[test]
fn over_pressure_faults_the_machine_and_only_service_clears_it() {
    let mut c = ready();
    let mut s = warm();
    c.handle(1000, &s, Event::ShotRequested).unwrap();
    s.brew_bar = 11.5;
    c.tick(DT, 1025, &s);
    assert_eq!(c.state, MachineState::Faulted);
    assert_eq!(c.act, Actuators::IDLE);
    assert_eq!(c.faults().len(), 1);
    assert_eq!(c.faults()[0].kind, FaultKind::OverPressure);
    assert_eq!(c.last_shot.unwrap().state, ShotState::Aborted);
    assert_eq!(c.last_shot.unwrap().final_grams, None);

    // A faulted machine ignores time passing.
    let frozen = c;
    c.tick(DT, 1050, &warm());
    assert_eq!(c, frozen);

    assert_eq!(
        c.handle(2000, &s, Event::FaultsCleared(ClientKind::Service)),
        Err(Reject::StillOverPressure)
    );
    assert_eq!(
        c.handle(2000, &warm(), Event::FaultsCleared(ClientKind::Remote)),
        Err(Reject::OnlyServiceClears)
    );
    assert_eq!(
        c.handle(2000, &warm(), Event::WakeRequested(PowerMode::Full)),
        Err(Reject::Faulted)
    );
    c.handle(2000, &warm(), Event::FaultsCleared(ClientKind::Service))
        .unwrap();
    assert_eq!(c.state, MachineState::Sleeping);
    assert_eq!(c.faults()[0].cleared_at, Some(2000));
}

#[test]
fn hot_mix_water_and_a_lost_inlet_abort_a_shot() {
    let mut hot = ready();
    let mut s = warm();
    hot.handle(1000, &s, Event::ShotRequested).unwrap();
    s.mix_c = 99.5;
    hot.tick(DT, 1025, &s);
    assert_eq!(hot.faults()[0].kind, FaultKind::MixOverTemperature);

    let mut dry = ready();
    let mut s = warm();
    dry.handle(1000, &s, Event::ShotRequested).unwrap();
    s.inlet_bar = 0.2;
    dry.tick(DT, 1025, &s);
    assert_eq!(dry.faults()[0].kind, FaultKind::NoInletPressure);
}

#[test]
fn the_fault_log_keeps_the_newest_eight() {
    let mut c = Controller::new(config(), tuning(), classic());
    let kinds = [
        FaultKind::SensorOpen,
        FaultKind::SensorShort,
        FaultKind::ScaleLost,
        FaultKind::Watchdog,
    ];
    for i in 0..11u64 {
        c.handle(i * 10, &warm(), Event::SensorFailed(kinds[i as usize % 4]))
            .unwrap();
        c.handle(i * 10 + 5, &warm(), Event::FaultsCleared(ClientKind::Service))
            .unwrap();
    }
    assert_eq!(c.faults().len(), MAX_FAULTS);
    assert_eq!(c.faults()[0].raised_at, 30);
    assert_eq!(c.faults()[MAX_FAULTS - 1].raised_at, 100);
    assert_eq!(
        c.handle(200, &warm(), Event::SensorFailed(FaultKind::OverPressure)),
        Err(Reject::NotASensorFailure)
    );
}

#[test]
fn the_steam_boiler_fills_before_it_heats_and_a_fill_can_time_out() {
    let mut c = Controller::new(config(), tuning(), classic());
    let mut s = warm();
    s.steam_level_ok = false;
    s.steam_bar = 0.0;
    c.handle(0, &s, Event::WakeRequested(PowerMode::Full)).unwrap();
    c.tick(DT, 25, &s);
    assert_eq!(c.act.fill_valve, TwoWay::Open);
    assert!(!c.act.steam_element_on);
    assert!(c.handle(50, &s, Event::ShotRequested).is_err());
    s.steam_level_ok = true;
    c.tick(DT, 5000, &s);
    assert_eq!(c.act.fill_valve, TwoWay::Closed);
    assert_eq!(c.fills_since_blowdown, 1);

    let mut stuck = Controller::new(config(), tuning(), classic());
    s.steam_level_ok = false;
    stuck.handle(0, &s, Event::WakeRequested(PowerMode::Full)).unwrap();
    stuck.tick(DT, 25, &s);
    stuck.tick(DT, 90_000, &s);
    assert_eq!(stuck.state, MachineState::Heating);
    stuck.tick(DT, 90_025, &s);
    assert_eq!(stuck.state, MachineState::Faulted);
    assert_eq!(stuck.faults()[0].kind, FaultKind::FillTimeout);
}

#[test]
fn an_idle_machine_goes_to_sleep_on_time() {
    let mut c = ready();
    c.tick(DT, 2_700_000, &warm());
    assert_eq!(c.state, MachineState::Ready);
    c.tick(DT, 2_700_025, &warm());
    assert_eq!(c.state, MachineState::Sleeping);
    assert_eq!(c.act, Actuators::IDLE);
}

#[test]
fn time_keeps_its_milliseconds_after_weeks_of_uptime() {
    let month: Ms = 30 * 24 * 3600 * 1000;
    assert_eq!(elapsed(month, month + 25), 0.025);
    assert_eq!(core_wire::to_ms(month as f64 / 1000.0 + 0.025), month + 25);
    assert_eq!(elapsed(100, 50), 0.0);
}

#[test]
fn a_profile_has_between_one_and_twelve_steps() {
    assert!(Profile::new(36.0, 92.0, 93.0, None, &[]).is_none());
    assert!(Profile::new(36.0, 92.0, 93.0, None, &[hold(); MAX_STEPS]).is_some());
    assert!(Profile::new(36.0, 92.0, 93.0, None, &[hold(); MAX_STEPS + 1]).is_none());
    assert_eq!(classic().steps().len(), 2);
    assert_eq!(classic().step(2), None);
}

fn step_wire(s: &Step) -> [f64; core_wire::STEP_LEN] {
    let opt = |v: Option<Real>| v.map_or(f64::NAN, |x| x as f64);
    [
        s.mode as u8 as f64,
        s.curve as u8 as f64,
        s.ramp_s as f64,
        s.target_c as f64,
        opt(s.target_bar),
        opt(s.target_ml_per_s),
        s.max_duration_s as f64,
        opt(s.exit_above_bar),
        opt(s.exit_above_grams),
    ]
}

fn profile_wire(p: &Profile) -> std::vec::Vec<f64> {
    let mut out = std::vec![
        p.target_grams as f64,
        p.group_base_c as f64,
        p.max_target_c as f64,
        p.target_seconds.map_or(f64::NAN, |x| x as f64),
        p.steps().len() as f64,
    ];
    for step in p.steps() {
        out.extend_from_slice(&step_wire(step));
    }
    out
}

const WARM_WIRE: [f64; core_wire::SENSORS_LEN] = [3.0, 90.0, 15.0, 0.0, 104.0, 92.0, 92.0, 92.0, 1.8, 1.0, 0.0];

#[test]
fn messages_read_back_what_was_written() {
    let wire = profile_wire(&classic());
    assert_eq!(Reader::new(&wire).profile(), Ok(classic()));
    assert_eq!(Reader::new(&WARM_WIRE).sensors(), Ok(warm()));
    let event = [2.0];
    assert_eq!(
        Reader::new(&event).event(core_wire::EV_CLEAR_FAULTS),
        Ok(Event::FaultsCleared(ClientKind::Service))
    );
}

#[test]
fn a_message_that_does_not_fit_is_refused_whole() {
    let mut wire = profile_wire(&classic());
    // a step mode that does not exist
    wire[core_wire::PROFILE_HEAD_LEN] = 7.0;
    assert_eq!(Reader::new(&wire).profile(), Err(Reject::Malformed));
    // a count with no steps behind it
    let mut wire = profile_wire(&classic());
    wire.truncate(core_wire::PROFILE_HEAD_LEN + 3);
    assert_eq!(Reader::new(&wire).profile(), Err(Reject::Malformed));
    // thirteen steps
    let mut wire = profile_wire(&classic());
    wire[4] = 13.0;
    assert_eq!(Reader::new(&wire).profile(), Err(Reject::Malformed));
    // an infinite temperature, a fractional flag, a mode out of range
    let mut bad = WARM_WIRE;
    bad[4] = f64::INFINITY;
    assert_eq!(Reader::new(&bad).sensors(), Err(Reject::Malformed));
    let mut bad = WARM_WIRE;
    bad[9] = 0.5;
    assert_eq!(Reader::new(&bad).sensors(), Err(Reject::Malformed));
    assert_eq!(Reader::new(&[3.0]).event(core_wire::EV_WAKE), Err(Reject::Malformed));
    assert_eq!(Reader::new(&[]).event(99), Err(Reject::Malformed));

    // A refused operation leaves the controller as it was.
    let mut c = ready();
    let before = c;
    assert_eq!(
        core_wire::apply(&mut c, core_wire::OP_TICK, 0.025, 1.0, &bad),
        Err(Reject::Malformed)
    );
    assert_eq!(
        core_wire::apply(&mut c, core_wire::OP_TICK, 0.0, 1.0, &WARM_WIRE),
        Err(Reject::Malformed)
    );
    assert_eq!(
        core_wire::apply(&mut c, 42, 0.0, 0.0, &WARM_WIRE),
        Err(Reject::Malformed)
    );
    assert_eq!(c, before);
}

#[test]
fn the_status_reports_the_shot_and_the_faults() {
    let mut c = ready();
    let mut out = [0.0f64; core_wire::STATUS_LEN];
    let mut message = std::vec::Vec::from(WARM_WIRE);
    assert_eq!(
        core_wire::apply(&mut c, core_wire::OP_EVENT, core_wire::EV_SHOT as f64, 1.0, &message),
        Ok(())
    );
    core_wire::write_status(&c, &mut out);
    assert_eq!(out[1], MachineState::Ready as u8 as f64);
    assert_eq!((out[21], out[22], out[23]), (1.0, 0.0, 1.0));
    assert!(out[15].is_nan(), "a flow step has no pressure setpoint");
    assert_eq!(out[16], 2.0);
    message[3] = 11.5;
    assert_eq!(
        core_wire::apply(&mut c, core_wire::OP_TICK, 0.025, 1.025, &message),
        Ok(())
    );
    core_wire::write_status(&c, &mut out);
    assert_eq!(out[1], MachineState::Faulted as u8 as f64);
    assert_eq!((out[21], out[25], out[26]), (0.0, 1.0, ShotState::Aborted as u8 as f64));
    assert!(out[29].is_nan(), "an aborted shot has no final weight");
    assert_eq!(out[32], 1.0);
    assert_eq!(out[33], FaultKind::OverPressure as u8 as f64);
    assert_eq!(out[34], 1.025);
    assert!(out[35].is_nan(), "the fault has not been cleared");
}

#[test]
fn every_refusal_has_its_own_number_and_words() {
    for (i, reason) in Reject::ALL.iter().enumerate() {
        assert_eq!(*reason as usize, i + 1);
        assert!(!reason.text().is_empty());
    }
}
