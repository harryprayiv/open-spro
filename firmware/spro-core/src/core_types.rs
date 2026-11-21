//! Vocabulary. Names follow `spec/machine.allium`.

/// Every measured or computed quantity. Single precision, because the
/// ESP32-C6 has no floating point unit and the simulator must compute
/// the same bits the machine does.
pub type Real = f32;

/// Time since power-on, in milliseconds. A float would lose whole
/// milliseconds after a few hours of uptime.
pub type Ms = u64;

/// Seconds from `from` to `to`, and zero if `to` is the earlier.
pub fn elapsed(from: Ms, to: Ms) -> Real {
    to.saturating_sub(from) as Real / 1000.0
}

/// `x` held between `lo` and `hi`.
pub fn clamp(lo: Real, hi: Real, x: Real) -> Real {
    let floor = if x < lo { lo } else { x };
    if floor > hi {
        hi
    } else {
        floor
    }
}

pub fn max(a: Real, b: Real) -> Real {
    if a > b {
        a
    } else {
        b
    }
}

pub fn min(a: Real, b: Real) -> Real {
    if a < b {
        a
    } else {
        b
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PowerMode {
    Asleep = 0,
    BrewReady = 1,
    Full = 2,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MachineState {
    Sleeping = 0,
    Heating = 1,
    Ready = 2,
    Faulted = 3,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TwoWay {
    Closed = 0,
    Open = 1,
}

/// SV_GRP.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GroupValve {
    ToDrain = 0,
    ToGroup = 1,
}

/// MV_MIX.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MixValve {
    Parked = 0,
    Regulating = 1,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PumpMode {
    Stopped = 0,
    PressureControl = 1,
    FlowControl = 2,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum StepMode {
    LinePrebrew = 0,
    Pressure = 1,
    Flow = 2,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ShotState {
    Running = 0,
    StoppedByWeight = 1,
    StoppedByTime = 2,
    StoppedByUser = 3,
    Aborted = 4,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FaultKind {
    SensorOpen = 0,
    SensorShort = 1,
    OverTemperature = 2,
    OverPressure = 3,
    FillTimeout = 4,
    NoInletPressure = 5,
    ScaleLost = 6,
    Watchdog = 7,
    MixOverTemperature = 8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ClientKind {
    Paddle = 0,
    Remote = 1,
    Service = 2,
    Supervisor = 3,
}

/// How a step's setpoints travel from where they stood when the step
/// began to the step's own targets.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Curve {
    Jump = 0,
    Linear = 1,
    Ease = 2,
    Exponential = 3,
}

/// One step of a profile.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Step {
    pub mode: StepMode,
    pub curve: Curve,
    /// Seconds the setpoints take to reach this step's targets.
    pub ramp_s: Real,
    pub target_c: Real,
    pub target_bar: Option<Real>,
    pub target_ml_per_s: Option<Real>,
    pub max_duration_s: Real,
    pub exit_above_bar: Option<Real>,
    pub exit_above_grams: Option<Real>,
}

impl Step {
    /// A step that holds line pressure for no time. It fills the unused
    /// places of a profile and is never run.
    pub const EMPTY: Step = Step {
        mode: StepMode::LinePrebrew,
        curve: Curve::Jump,
        ramp_s: 0.0,
        target_c: 0.0,
        target_bar: None,
        target_ml_per_s: None,
        max_duration_s: 0.0,
        exit_above_bar: None,
        exit_above_grams: None,
    };
}

/// The most steps a profile can have. The controller has no allocator,
/// so a profile is a fixed block of memory.
pub const MAX_STEPS: usize = 12;

/// A profile has between one and `MAX_STEPS` steps. `Profile::new`
/// refuses anything else, so a profile that exists always has a first
/// step.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Profile {
    pub target_grams: Real,
    pub group_base_c: Real,
    pub max_target_c: Real,
    /// How long the shot should take to reach its weight. When present,
    /// the machine paces the shot towards it.
    pub target_seconds: Option<Real>,
    steps: [Step; MAX_STEPS],
    count: usize,
}

impl Profile {
    pub fn new(
        target_grams: Real,
        group_base_c: Real,
        max_target_c: Real,
        target_seconds: Option<Real>,
        steps: &[Step],
    ) -> Option<Profile> {
        if steps.is_empty() || steps.len() > MAX_STEPS {
            return None;
        }
        let mut held = [Step::EMPTY; MAX_STEPS];
        held[..steps.len()].copy_from_slice(steps);
        Some(Profile {
            target_grams,
            group_base_c,
            max_target_c,
            target_seconds,
            steps: held,
            count: steps.len(),
        })
    }

    pub fn steps(&self) -> &[Step] {
        &self.steps[..self.count]
    }

    pub fn first(&self) -> Step {
        self.steps[0]
    }

    pub fn step(&self, index: usize) -> Option<Step> {
        self.steps().get(index).copied()
    }
}

/// The running shot. It runs the selected profile, which cannot change
/// while a shot is running, so the shot holds only its place in it.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Shot {
    pub step_index: usize,
    pub step: Step,
    pub started_at: Ms,
    pub step_started_at: Ms,
}

/// What is kept of a shot once it has ended.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ShotRecord {
    pub state: ShotState,
    pub step_index: usize,
    pub started_at: Ms,
    pub ended_at: Ms,
    /// Absent for a shot a fault aborted.
    pub final_grams: Option<Real>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Fault {
    pub kind: FaultKind,
    pub raised_at: Ms,
    pub cleared_at: Option<Ms>,
}

/// The most faults the controller remembers. When one more is raised
/// the oldest is forgotten.
pub const MAX_FAULTS: usize = 8;

/// What the firmware can read.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Sensors {
    pub inlet_bar: Real,
    pub inlet_conductivity_us: Real,
    pub inlet_c: Real,
    pub brew_bar: Real,
    pub brew_boiler_c: Real,
    pub mix_c: Real,
    pub group_c: Real,
    pub group_water_c: Real,
    pub steam_bar: Real,
    pub steam_level_ok: bool,
    pub cup_grams: Real,
}

/// What the firmware can drive. `pump_speed` and `mix_position` run
/// from 0 to 1 and only mean something in the matching pump and mix
/// modes.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Actuators {
    pub heat_enabled: bool,
    pub brew_element_on: bool,
    pub steam_element_on: bool,
    pub group_heater_on: bool,
    pub pump: PumpMode,
    pub pump_speed: Real,
    pub bypass_valve: TwoWay,
    pub fill_valve: TwoWay,
    pub group_valve: GroupValve,
    pub mix_valve: MixValve,
    pub mix_position: Real,
}

impl Actuators {
    pub const IDLE: Actuators = Actuators {
        heat_enabled: false,
        brew_element_on: false,
        steam_element_on: false,
        group_heater_on: false,
        pump: PumpMode::Stopped,
        pump_speed: 0.0,
        bypass_valve: TwoWay::Closed,
        fill_valve: TwoWay::Closed,
        group_valve: GroupValve::ToDrain,
        mix_valve: MixValve::Parked,
        mix_position: 0.0,
    };
}

/// The config block of the spec. Durations are in seconds.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Config {
    pub circuit_budget_watts: i32,
    pub brew_element_watts: i32,
    pub steam_element_watts: i32,
    pub group_heater_watts: i32,
    pub reservoir_target_c: Real,
    pub reservoir_headroom_c: Real,
    pub steam_target_bar: Real,
    pub ready_band_c: Real,
    pub drop_out_band_c: Real,
    pub steam_ready_band_bar: Real,
    pub steam_drop_out_band_bar: Real,
    pub brew_limit_c: Real,
    pub mix_limit_c: Real,
    pub group_limit_c: Real,
    pub steam_limit_bar: Real,
    pub brew_pressure_limit_bar: Real,
    /// The most the machine may move a pressure setpoint, either way,
    /// to pace a shot towards its target time.
    pub pace_limit_bar: Real,
    pub min_inlet_bar: Real,
    pub max_inlet_conductivity_us: Real,
    pub fill_timeout_s: Real,
    pub shot_timeout_s: Real,
    pub idle_sleep_after_s: Real,
    pub blowdown_after_fills: i32,
}

/// What the firmware knows about its own hardware and how hard its
/// loops push.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Tuning {
    // calibration
    pub pump_max_ml_per_s: Real,
    pub pump_slip_ml_per_s_bar: Real,
    pub needle_ml_per_s_bar: Real,
    pub cold_branch_max_ratio: Real,
    // pressure loop
    pub pump_kp: Real,
    pub pump_ki: Real,
    pub pump_integral_limit: Real,
    pub pump_integral_above_bar: Real,
    // mix loop
    pub mix_trim_gain: Real,
    pub mix_trim_share: Real,
    pub mix_trim_delay_s: Real,
    pub mix_min_flow_ml_per_s: Real,
    // pacing a shot towards its target time
    pub pace_gain: Real,
    pub pace_filter_s: Real,
    pub pace_start_grams: Real,
    // heater sharing
    pub heater_turn_s: Real,
}

/// What a client can ask of the machine. One event carries a whole
/// profile and the rest carry almost nothing. With no allocator there
/// is nowhere else to put the profile.
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Event {
    WakeRequested(PowerMode),
    SleepRequested,
    ProfileSelected(Profile),
    ShotRequested,
    StepAdvanceRequested,
    ShotStopRequested,
    FaultsCleared(ClientKind),
    BlowdownConfirmed,
    WaterWarningAcknowledged,
    SensorFailed(FaultKind),
}

/// Why the machine refused an event: the `requires` clause of the rule
/// that did not hold.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Reject {
    WakeNeedsMode = 1,
    Faulted = 2,
    NotAwake = 3,
    ShotRunning = 4,
    NotReady = 5,
    TemperaturesDrifted = 6,
    GroupSetForAnother = 7,
    ReservoirTooCool = 8,
    ProfileAboveMixLimit = 9,
    ShotAlreadyRunning = 10,
    SteamBoilerFilling = 11,
    NoInletPressure = 12,
    NoShotRunning = 13,
    NotFaulted = 14,
    OnlyServiceClears = 15,
    StillOverLimit = 16,
    StillOverPressure = 17,
    BlowdownNeedsSleep = 18,
    WaterStillOutOfRange = 19,
    AlreadyFaulted = 20,
    NotASensorFailure = 21,
    /// The message could not be read: a number out of range for the
    /// field it fills, or a profile with no steps or too many.
    Malformed = 22,
}

impl Reject {
    pub const ALL: [Reject; 22] = [
        Reject::WakeNeedsMode,
        Reject::Faulted,
        Reject::NotAwake,
        Reject::ShotRunning,
        Reject::NotReady,
        Reject::TemperaturesDrifted,
        Reject::GroupSetForAnother,
        Reject::ReservoirTooCool,
        Reject::ProfileAboveMixLimit,
        Reject::ShotAlreadyRunning,
        Reject::SteamBoilerFilling,
        Reject::NoInletPressure,
        Reject::NoShotRunning,
        Reject::NotFaulted,
        Reject::OnlyServiceClears,
        Reject::StillOverLimit,
        Reject::StillOverPressure,
        Reject::BlowdownNeedsSleep,
        Reject::WaterStillOutOfRange,
        Reject::AlreadyFaulted,
        Reject::NotASensorFailure,
        Reject::Malformed,
    ];

    pub fn text(self) -> &'static str {
        match self {
            Reject::WakeNeedsMode => "wake needs brew_ready or full",
            Reject::Faulted => "machine is faulted",
            Reject::NotAwake => "machine is not awake",
            Reject::ShotRunning => "a shot is running",
            Reject::NotReady => "machine is not ready",
            Reject::TemperaturesDrifted => "temperatures have drifted out of range",
            Reject::GroupSetForAnother => "group is set for another profile",
            Reject::ReservoirTooCool => "reservoir is not hot enough for this profile",
            Reject::ProfileAboveMixLimit => "profile asks for water above the mix limit",
            Reject::ShotAlreadyRunning => "a shot is already running",
            Reject::SteamBoilerFilling => "steam boiler is filling",
            Reject::NoInletPressure => "no inlet pressure",
            Reject::NoShotRunning => "no shot is running",
            Reject::NotFaulted => "machine is not faulted",
            Reject::OnlyServiceClears => "only the service panel clears faults",
            Reject::StillOverLimit => "a temperature or steam pressure is still over its limit",
            Reject::StillOverPressure => "brew pressure is still over its limit",
            Reject::BlowdownNeedsSleep => "blowdown is confirmed with the machine asleep",
            Reject::WaterStillOutOfRange => "inlet water is still out of range",
            Reject::AlreadyFaulted => "machine is already faulted",
            Reject::NotASensorFailure => "not a sensor failure kind",
            Reject::Malformed => "the message could not be read, or the profile has no steps or more than twelve",
        }
    }
}
