module Util.Color exposing (interpolateFrom)

import Color exposing (Color)
import Util.Float


interpolateFrom : Color -> Color -> Float -> Color
interpolateFrom from to offset =
    let
        fromHsla =
            Color.toHsla from

        toHsla =
            Color.toHsla to
    in
    Color.fromHsla
        { hue = Util.Float.interpolateFrom fromHsla.hue toHsla.hue offset
        , saturation = Util.Float.interpolateFrom fromHsla.saturation toHsla.saturation offset
        , lightness = Util.Float.interpolateFrom fromHsla.lightness toHsla.lightness offset
        , alpha = Util.Float.interpolateFrom fromHsla.alpha toHsla.alpha offset
        }
