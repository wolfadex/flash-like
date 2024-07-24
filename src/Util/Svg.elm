module Util.Svg exposing (attributeIf)

import Svg
import Svg.Attributes


attributeIf : Bool -> Svg.Attribute msg -> Svg.Attribute msg
attributeIf condition attribute =
    if condition then
        attribute

    else
        Svg.Attributes.class ""
