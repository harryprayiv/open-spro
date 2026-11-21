//! The shapes a setpoint can follow on its way to a step's target.

use crate::core_types::{clamp, Curve, Real};

/// How far along the way to the target a setpoint is when the fraction
/// `raw` of the ramp time has passed. Every curve ends at 1.
pub fn shape(curve: Curve, raw: Real) -> Real {
    let x = clamp(0.0, 1.0, raw);
    match curve {
        Curve::Jump => 1.0,
        Curve::Linear => x,
        Curve::Ease => x * x * (3.0 - 2.0 * x),
        Curve::Exponential => {
            // The division is skipped at the end of the ramp so the
            // curve ends on exactly 1.
            if x >= 1.0 {
                1.0
            } else {
                (1.0 - libm::expf(-4.0 * x)) / (1.0 - libm::expf(-4.0))
            }
        }
    }
}
