module Util.Array exposing (insertAt, mapAt)

import Array exposing (Array)


insertAt : Int -> a -> Array a -> Array a
insertAt index a arr =
    Array.append
        (Array.slice 0 index arr)
        (Array.append
            (Array.fromList [ a ])
            (Array.slice index (Array.length arr) arr)
        )


mapAt : Int -> (a -> a) -> Array a -> Array a
mapAt index fn =
    Array.indexedMap
        (\i a ->
            if i == index then
                fn a

            else
                a
        )
