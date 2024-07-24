module Util.List exposing (updateOrPush)


updateOrPush : a -> (a -> Maybe a) -> List a -> List a
updateOrPush item update list =
    updateOrPushHelper item update list []


updateOrPushHelper : a -> (a -> Maybe a) -> List a -> List a -> List a
updateOrPushHelper item update list acc =
    case list of
        [] ->
            item :: List.reverse acc

        x :: xs ->
            case update x of
                Just updated ->
                    List.reverse acc ++ updated :: xs

                Nothing ->
                    updateOrPushHelper item update (x :: acc) xs
