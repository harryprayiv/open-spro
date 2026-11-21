-- | The shot as a timeline. Each step of the profile is a band, laid
-- | out by the forecast, and the marks between bands are where one step
-- | hands over to the next. The dashed line is the pressure the profile
-- | asks for. The solid line is the pressure the machine has been asked
-- | for so far in this shot: on a profile it follows the dashed one,
-- | and in manual mode it is the curve the barista has drawn with the
-- | fader. The playhead is the upright line that travels with the shot.
module Sim.View.Playhead (playhead) where

import Prelude

import Data.Array (catMaybes, concat, index, mapWithIndex, range)
import Data.Array.NonEmpty as NEA
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (joinWith)
import Halogen.HTML as HH
import Sim.Link (Event(..), FaderMode(..))
import Sim.Model (Action(..), State)
import Sim.Recipe (toProfile)
import Sim.Update (allShapes, playheadAt, segments, timelineSpan)
import Sim.View.Svg (at, label, line, num, polyline, rect, svg)
import Sim.View.Widgets (button, cls, n1)

width :: Number
width = 600.0

height :: Number
height = 64.0

playhead :: forall w. State -> HH.HTML w Action
playhead st =
  let
    span = timelineSpan st
    place = playheadAt st
    cfg = st.params.config
    names = map _.name (NEA.toArray (toProfile (allShapes st) st.recipe).steps)
    shot = st.machine.status.shot
    manual = shot >>= _.manual
    manualC = shot >>= _.manualC
    held = isJust manual || isJust manualC
    mode = st.machine.status.faderMode
    x t = t / span * width
    y bar = height - clamp 0.0 12.0 bar / 12.0 * (height - 12.0)
    now = map _.stepIndex shot
    band i seg =
      [ rect
          [ at "class" ("band" <> (if now == Just i && not (isJust manual) then " open" else if i `mod` 2 == 1 then " odd" else ""))
          , at "x" (num (x seg.start)), at "y" "0"
          , at "width" (num (max 0.0 (x seg.end - x seg.start))), at "height" (num height)
          ]
      , line [ at "class" "key", at "x1" (num (x seg.start)), at "x2" (num (x seg.start)), at "y1" "0", at "y2" (num height) ]
      , label [ at "class" "band-name", at "x" (num (x seg.start + 4.0)), at "y" "9" ] (show (i + 1) <> " " <> fromMaybe "" (index names i))
      ]
    points pick samples = joinWith " " (catMaybes (map (\s -> map (\bar -> num (x s.t) <> "," <> num (y bar)) (pick s)) samples))
    tick i = label [ at "class" "axis", at "x" (num (x (toNumber i * 15.0) + 2.0)), at "y" (num (height - 2.0)) ] (show (i * 15) <> " s")
    -- In manual mode the limits the machine holds the fader to.
    limits = case manual of
      Just _ ->
        [ line [ at "class" "limit", at "x1" "0", at "x2" (num width), at "y1" (num (y cfg.manualMaxBar)), at "y2" (num (y cfg.manualMaxBar)) ]
        , line [ at "class" "limit", at "x1" "0", at "x2" (num width), at "y1" (num (y cfg.manualMinBar)), at "y2" (num (y cfg.manualMinBar)) ]
        ]
      Nothing -> []
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
      [ svg [ at "viewBox" ("0 0 " <> num width <> " " <> num height), at "class" "chart timeline", at "preserveAspectRatio" "none" ]
          ( concat (mapWithIndex band (segments st))
              <> map tick (range 0 (floor (span / 15.0) - 1))
              <> limits
              <>
                [ polyline [ at "class" "target", at "fill" "none", at "points" (points _.askedBar st.forecast.samples) ]
                , polyline [ at "class" "drawn", at "fill" "none", at "points" (points _.askedBar st.machine.trace) ]
                , line [ at "class" "play-line", at "x1" (num (x place)), at "x2" (num (x place)), at "y1" "0", at "y2" (num height) ]
                ]
          )
      , HH.div [ cls "playhead-words" ]
          ( [ HH.p [ cls "note" ] [ HH.text words ] ]
              <> (if held then [ button "Follow the profile" (Send FaderGiven) ] else [])
          )
      ]
