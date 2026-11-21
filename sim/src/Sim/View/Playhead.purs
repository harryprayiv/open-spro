-- | The shot as a timeline. The drawing of it is the e-ink display on
-- | the front of the machine (`Sim.View.Front`), and this module works
-- | out what that display shows: the profile's steps as bands, the
-- | pressure the profile will ask for, the pressure that has been asked
-- | for so far, and how far the shot has run. Under the drawings the
-- | page says in words what the fader is doing, and offers the fader
-- | back to the profile while a hand holds it.
module Sim.View.Playhead
  ( playhead
  , inkOf
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..), isJust)
import Halogen.HTML as HH
import Sim.Link (Event(..), FaderMode(..))
import Sim.Model (Action(..), State)
import Sim.Update (playheadAt, segments, timelineSpan)
import Sim.View.Front (Ink)
import Sim.View.Widgets (button, cls, n1)

-- | What the e-ink display shows.
inkOf :: State -> Ink
inkOf st =
  let
    cfg = st.params.config
    shot = st.machine.status.shot
    asked samples = mapMaybe (\s -> map (\bar -> { t: s.t, bar }) s.askedBar) samples
  in
    { span: timelineSpan st
    , place: playheadAt st
    , running: isJust shot
    , bands: map (\seg -> { start: seg.start, end: seg.end }) (segments st)
    -- With the pressure held by hand the steps stand still, so none of
    -- them is marked as the one the shot is in.
    , step: case shot of
        Just running | not (isJust running.manual) -> Just running.stepIndex
        _ -> Nothing
    , target: asked st.forecast.samples
    , drawn: asked st.machine.trace
    , limits: case shot >>= _.manual of
        Just _ -> Just { low: cfg.manualMinBar, high: cfg.manualMaxBar }
        Nothing -> Nothing
    }

playhead :: forall w. State -> HH.HTML w Action
playhead st =
  let
    cfg = st.params.config
    shot = st.machine.status.shot
    manual = shot >>= _.manual
    manualC = shot >>= _.manualC
    held = isJust manual || isJust manualC
    mode = st.machine.status.faderMode
    words = case shot, manual, manualC of
      Just _, Just bar, _ ->
        "By hand at " <> n1 bar <> " bar. The fader is the pressure, held between " <> n1 cfg.manualMinBar <> " and "
          <> n1 cfg.manualMaxBar <> " bar. The shot stops on its weight, or when you stop it."
      Just running, _, Just celsius ->
        "Water by hand at " <> n1 celsius <> " C, held between " <> n1 cfg.manualMinC <> " and " <> n1 cfg.manualMaxC
          <> " C. The profile carries on with its pressures, now at step " <> show (running.stepIndex + 1) <> "."
      Just running, Nothing, Nothing ->
        "Following the profile, step " <> show (running.stepIndex + 1) <> ". " <> case mode of
          OnPressure -> "Take the fader to carry on by hand."
          OnTemperature -> "Take the fader to set the water by hand."
          OnTime -> "The fader is showing the time and cannot be moved."
      Nothing, _, _ -> case mode of
        OnPressure -> "Press the button on the group to run the profile, or push the fader up to pull the shot by hand."
        OnTemperature -> "Press the button on the group to run the profile. During the shot the fader sets the water temperature."
        OnTime -> "Press the button on the group to run the profile. The fader will show how far the shot has run."
  in
    HH.div [ cls ("playhead" <> (if held then " by-hand" else "") <> (if isJust shot then " running" else "")) ]
      [ HH.div [ cls "playhead-words" ]
          ( [ HH.p [ cls "note" ] [ HH.text words ] ]
              <> (if held then [ button "Follow the profile" (Send FaderGiven) ] else [])
          )
      ]
