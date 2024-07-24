module Util.Float exposing (..)

{-| -}


{-| Interpolate from the first value to the second, based on a parameter that
ranges from zero to one. Passing a parameter value of zero will return the start
value and passing a parameter value of one will return the end value.

    Float.interpolateFrom 5 10 0
    --> 5

    Float.interpolateFrom 5 10 1
    --> 10

    Float.interpolateFrom 5 10 0.6
    --> 8

The end value can be less than the start value:

    Float.interpolateFrom 10 5 0.1
    --> 9.5

Parameter values less than zero or greater than one can be used to extrapolate:

    Float.interpolateFrom 5 10 1.5
    --> 12.5

    Float.interpolateFrom 5 10 -0.5
    --> 2.5

    Float.interpolateFrom 10 5 -0.2
    --> 11

-}
interpolateFrom : Float -> Float -> Float -> Float
interpolateFrom start end parameter =
    if parameter <= 0.5 then
        start + parameter * (end - start)

    else
        end + (1 - parameter) * (start - end)
