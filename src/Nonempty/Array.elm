module Nonempty.Array exposing (..)

import Array exposing (Array)
import Util.Array


type NonemptyArray a
    = NonemptyArray ( a, Array a )


singleton : a -> NonemptyArray a
singleton a =
    NonemptyArray ( a, Array.empty )


fromTuple : ( a, Array a ) -> NonemptyArray a
fromTuple =
    NonemptyArray


map : (a -> b) -> NonemptyArray a -> NonemptyArray b
map fn (NonemptyArray ( a, rest )) =
    NonemptyArray ( fn a, Array.map fn rest )


mapAt : Int -> (a -> a) -> NonemptyArray a -> NonemptyArray a
mapAt index fn (NonemptyArray ( a, rest )) =
    if index == 0 then
        NonemptyArray ( fn a, rest )

    else
        NonemptyArray ( a, Util.Array.mapAt (index - 1) fn rest )


uncons : NonemptyArray a -> ( a, Array a )
uncons (NonemptyArray items) =
    items


set : Int -> a -> NonemptyArray a -> NonemptyArray a
set index newItem (NonemptyArray ( a, rest )) =
    if index == 0 then
        NonemptyArray ( newItem, rest )

    else
        NonemptyArray ( a, Array.set (index - 1) newItem rest )


toList : NonemptyArray a -> List a
toList (NonemptyArray ( a, rest )) =
    a :: Array.toList rest


insertAt : Int -> a -> NonemptyArray a -> NonemptyArray a
insertAt index value (NonemptyArray ( first, rest )) =
    if index == 0 then
        NonemptyArray
            ( value
            , Array.append
                (Array.fromList [ first ])
                rest
            )

    else
        NonemptyArray
            ( first
            , Util.Array.insertAt (index - 1) value rest
            )


foldr : (a -> b -> b) -> b -> NonemptyArray a -> b
foldr fn initial (NonemptyArray ( a, rest )) =
    fn a (Array.foldr fn initial rest)
