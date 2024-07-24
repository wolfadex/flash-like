module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Css
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Nonempty.Array exposing (NonemptyArray)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Util.Color
import Util.Float
import Util.List
import Util.Svg
import Vector2d exposing (Vector2d)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-----------
-- MODEL --
-----------


type alias Model =
    { dragging : Maybe Dragging
    , selected : List Index
    , shapes : Array (NonemptyArray ShapeInstance)
    , groups : List (Set Int)
    , globalOffset : Float
    }


type alias ShapeInstance =
    { shape : Shape
    , offset : Float
    }


type Shape
    = Empty
    | LineSegment
        { p1 : Point
        , p2 : Point
        , color : Color
        }


type alias Point =
    Point2d Pixels CanvasCoordinates


type alias Dragging =
    { pointerId : PointerId
    , index : Index
    , point : Point2d Pixels CanvasCoordinates
    }


type alias Index =
    ( Int, Maybe Int )


type alias PointerId =
    Json.Encode.Value


type ScreenCoordinates
    = ScreenCoordinates Never


type CanvasCoordinates
    = CanvasCoordinates Never



----------
-- INIT --
----------


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dragging = Nothing
      , selected = []
      , globalOffset = 0
      , shapes =
            Array.fromList
                [ Nonempty.Array.singleton
                    { shape =
                        LineSegment
                            { p1 = Point2d.pixels 100 100
                            , p2 = Point2d.pixels 200 200
                            , color = Color.blue
                            }
                    , offset = 0
                    }
                , Nonempty.Array.fromTuple
                    ( { shape =
                            LineSegment
                                { p1 = Point2d.pixels 300 100
                                , p2 = Point2d.pixels 200 400
                                , color = Color.blue
                                }
                      , offset = 0
                      }
                    , Array.fromList
                        [ { shape =
                                LineSegment
                                    { p1 = Point2d.pixels 300 100
                                    , p2 = Point2d.pixels 400 400
                                    , color = Color.red
                                    }
                          , offset = 30
                          }
                        ]
                    )
                ]
      , groups = []
      }
    , Cmd.none
    )



-------------------
-- SUBSCRIPTIONS --
-------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown decodeKeyDown



------------
-- UPDATE --
------------


type Msg
    = UserClickedCanvas
    | UserReleasedPointerOnCanvas
    | UserPressedPointerWithinCanvas Index PointerId Bool (Point2d Pixels CanvasCoordinates)
    | UserMovedPoint Dragging (Point2d Pixels CanvasCoordinates)
    | UserPressedKeyDown String { ctrlKey : Bool }
    | UserSelectedColor String
    | UserScrobbledTimeline Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserClickedCanvas ->
            ( { model | selected = [] }
            , Cmd.none
            )

        UserScrobbledTimeline globalOffset ->
            ( { model | globalOffset = globalOffset }
            , Cmd.none
            )

        UserPressedPointerWithinCanvas (( start, _ ) as index) pointerId shiftKey point ->
            ( { model
                | selected =
                    let
                        selection =
                            ( start, Nothing )
                    in
                    if List.member selection model.selected then
                        model.selected

                    else if shiftKey then
                        selection :: model.selected

                    else
                        [ selection ]
                , dragging =
                    Just
                        { pointerId = pointerId
                        , index = index
                        , point = point
                        }
              }
            , Cmd.none
            )

        UserReleasedPointerOnCanvas ->
            ( { model | dragging = Nothing }
            , Cmd.none
            )

        UserMovedPoint dragging movedPoint ->
            ( { model
                | dragging =
                    Maybe.map
                        (\drag ->
                            { drag
                                | point = movedPoint
                            }
                        )
                        model.dragging
                , shapes =
                    List.foldl
                        (\( indexStart, indexDepth ) shapes ->
                            case Array.get indexStart shapes of
                                Nothing ->
                                    shapes

                                Just shapeTimeline ->
                                    case getShapeInstanceAtTime model.globalOffset shapeTimeline of
                                        Nothing ->
                                            shapes

                                        Just ( shapeInstance, instanceIndex ) ->
                                            let
                                                movedShape : Shape
                                                movedShape =
                                                    moveShape
                                                        (Tuple.second dragging.index)
                                                        dragging.point
                                                        movedPoint
                                                        shapeInstance.shape

                                                carl : NonemptyArray ShapeInstance
                                                carl =
                                                    Nonempty.Array.set instanceIndex
                                                        { shape = movedShape
                                                        , offset = shapeInstance.offset
                                                        }
                                                        shapeTimeline
                                            in
                                            Array.set
                                                indexStart
                                                carl
                                                shapes
                        )
                        model.shapes
                        model.selected
              }
            , Cmd.none
            )

        UserPressedKeyDown key modifiers ->
            case key of
                "g" ->
                    if modifiers.ctrlKey then
                        ( { model
                            | groups =
                                let
                                    newGroup =
                                        model.selected
                                            |> List.map Tuple.first
                                            |> Set.fromList
                                in
                                Util.List.updateOrPush
                                    newGroup
                                    (\group ->
                                        if Set.isEmpty (Set.intersect newGroup group) then
                                            Just (Set.union newGroup group)

                                        else
                                            Nothing
                                    )
                                    model.groups
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                "u" ->
                    if modifiers.ctrlKey then
                        let
                            selectionGroup =
                                model.selected
                                    |> List.map Tuple.first
                                    |> Set.fromList
                        in
                        ( { model
                            | groups =
                                List.filter
                                    (\group ->
                                        selectionGroup /= group
                                    )
                                    model.groups
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                "k" ->
                    if List.isEmpty model.selected then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | shapes =
                                List.foldl
                                    (\( indexStart, indexDepth ) shapes ->
                                        case Array.get indexStart shapes of
                                            Nothing ->
                                                shapes

                                            Just shapeTimeline ->
                                                case getShapeAfterTime model.globalOffset shapeTimeline of
                                                    NoShapeFound ->
                                                        shapes

                                                    KeyedShapeAlreadyAtTime ->
                                                        shapes

                                                    InterpolatedShape newInterpolatedShape ->
                                                        let
                                                            newTimeline : NonemptyArray ShapeInstance
                                                            newTimeline =
                                                                shapeTimeline
                                                                    |> Nonempty.Array.mapAt (newInterpolatedShape.index + 1)
                                                                        (\instance ->
                                                                            { instance
                                                                                | offset = instance.offset - newInterpolatedShape.offset
                                                                            }
                                                                        )
                                                                    |> Nonempty.Array.insertAt newInterpolatedShape.index
                                                                        { shape = newInterpolatedShape.shape
                                                                        , offset = newInterpolatedShape.offset
                                                                        }
                                                        in
                                                        Array.set indexStart
                                                            newTimeline
                                                            shapes
                                    )
                                    model.shapes
                                    model.selected
                          }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        UserSelectedColor colorStr ->
            let
                _ =
                    5
            in
            ( model, Cmd.none )


moveShape : Maybe Int -> Point -> Point -> Shape -> Shape
moveShape indexDepth start end shape =
    case ( shape, indexDepth ) of
        ( Empty, _ ) ->
            shape

        ( LineSegment segment, Nothing ) ->
            let
                vector =
                    Vector2d.from start end
            in
            LineSegment
                { segment
                    | p1 =
                        segment.p1
                            |> Point2d.translateBy
                                vector
                    , p2 =
                        segment.p2
                            |> Point2d.translateBy
                                vector
                }

        ( LineSegment segment, Just 0 ) ->
            LineSegment
                { segment
                    | p1 =
                        segment.p1
                            |> Point2d.translateBy
                                (Vector2d.from start end)
                }

        ( LineSegment segment, Just 1 ) ->
            LineSegment
                { segment
                    | p2 =
                        segment.p2
                            |> Point2d.translateBy
                                (Vector2d.from start end)
                }

        _ ->
            shape



----------
-- VIEW --
----------


view : Model -> Browser.Document Msg
view model =
    { title = "Hello World"
    , body =
        [ Html.h1 [] [ Html.text "Hello World" ]
        , Html.div
            [ Css.canvas
            ]
            [ model.shapes
                |> Array.foldl
                    (\shapeTimeline ( acc, index ) ->
                        ( case getShapeAtTime model.globalOffset shapeTimeline of
                            Nothing ->
                                acc

                            Just shape ->
                                viewShape False model.selected model.dragging index shape ++ acc
                        , index + 1
                        )
                    )
                    ( [], 0 )
                |> Tuple.first
                |> Svg.svg
                    ([ Svg.Attributes.width "800"
                     , Svg.Attributes.height "600"
                     , Svg.Attributes.viewBox "0 0 800 600"
                     , Html.Attributes.property "___capturePointer" <|
                        case model.dragging of
                            Nothing ->
                                Json.Encode.null

                            Just dragging ->
                                dragging.pointerId
                     , Svg.Events.stopPropagationOn "pointerdown" (Json.Decode.succeed ( UserClickedCanvas, True ))
                     , Svg.Events.on "pointerup" (decodePointerUp (\_ -> UserReleasedPointerOnCanvas))
                     ]
                        ++ (case model.dragging of
                                Nothing ->
                                    []

                                Just dragging ->
                                    [ Svg.Events.on "pointermove" (decodePointerMove (UserMovedPoint dragging))
                                    ]
                           )
                    )
            ]
        , model.shapes
            |> Array.toList
            |> List.indexedMap
                (\index shape ->
                    Html.div []
                        [ Html.input
                            [ Html.Attributes.type_ "range"
                            , Html.Attributes.list ("scrobbler-keys-" ++ String.fromInt index)
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "180"
                            , Html.Attributes.value (String.fromFloat model.globalOffset)
                            , Html.Events.onInput (String.toFloat >> Maybe.withDefault model.globalOffset >> UserScrobbledTimeline)
                            , Html.Attributes.style "width" "100%"
                            ]
                            []
                        , shape
                            |> Nonempty.Array.toList
                            |> List.foldl
                                (\instance ( options, offsetTotal ) ->
                                    ( Html.option
                                        [ Html.Attributes.value (String.fromFloat (instance.offset + offsetTotal)) ]
                                        []
                                        :: options
                                    , offsetTotal + instance.offset
                                    )
                                )
                                ( [], 0 )
                            |> Tuple.first
                            |> Html.datalist
                                [ Html.Attributes.id ("scrobbler-keys-" ++ String.fromInt index)
                                ]
                        ]
                )
            |> Html.div [ Css.column ]
        ]
    }


viewShape : Bool -> List Index -> Maybe Dragging -> Int -> Shape -> List (Svg Msg)
viewShape nested selected dragging index shape =
    case shape of
        Empty ->
            []

        LineSegment segment ->
            List.concat
                [ [ Geometry.Svg.lineSegment2d
                        [ Svg.Attributes.stroke (Color.toCssString segment.color)
                        , Svg.Attributes.strokeWidth "5"
                        , Svg.Attributes.style
                            ("cursor: "
                                ++ (case dragging of
                                        Just _ ->
                                            "grabbing"

                                        Nothing ->
                                            if List.member ( index, Nothing ) selected then
                                                "grab"

                                            else
                                                "pointer"
                                   )
                            )
                        , Svg.Events.stopPropagationOn "pointerdown" (decodePointerDown (UserPressedPointerWithinCanvas ( index, Nothing )))
                            |> Util.Svg.attributeIf (not nested)
                        , Svg.Events.on "pointerup" (decodePointerUp (\_ -> UserReleasedPointerOnCanvas))
                            |> Util.Svg.attributeIf (not nested)
                        ]
                        (LineSegment2d.from segment.p1 segment.p2)
                  ]
                , viewControlPoint
                    selected
                    dragging
                    ( index, Just 0 )
                    segment.p1
                , viewControlPoint
                    selected
                    dragging
                    ( index, Just 1 )
                    segment.p2
                ]


viewControlPoint : List Index -> Maybe Dragging -> Index -> Point2d Pixels CanvasCoordinates -> List (Svg Msg)
viewControlPoint selected maybeDragging ( index, depth ) point =
    if List.member ( index, Nothing ) selected then
        case maybeDragging of
            Nothing ->
                [ Geometry.Svg.circle2d
                    [ Svg.Attributes.fill "white"
                    , Svg.Attributes.stroke "white"
                    , Svg.Attributes.strokeWidth "1"
                    ]
                    (Circle2d.atPoint
                        point
                        (Pixels.pixels 7)
                    )
                , Geometry.Svg.circle2d
                    [ Svg.Attributes.fill "white"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Events.stopPropagationOn "pointerdown" (decodePointerDown (UserPressedPointerWithinCanvas ( index, depth )))
                    , Svg.Events.on "pointerup" (decodePointerUp (\_ -> UserReleasedPointerOnCanvas))
                    , Svg.Attributes.style "cursor: grab"
                    , Html.Attributes.property "___capturePointer" Json.Encode.null
                    ]
                    (Circle2d.atPoint
                        point
                        (Pixels.pixels 5)
                    )
                ]

            Just _ ->
                []

    else
        []


decodePointerDown : (PointerId -> Bool -> Point2d Pixels CanvasCoordinates -> msg) -> Json.Decode.Decoder ( msg, Bool )
decodePointerDown handler =
    Json.Decode.map4 (\pointerId x y shiftKey -> ( handler pointerId shiftKey (Point2d.pixels x y), True ))
        (Json.Decode.field "pointerId" Json.Decode.value)
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)
        (Json.Decode.field "shiftKey" Json.Decode.bool)


decodePointerUp : (PointerId -> msg) -> Json.Decode.Decoder msg
decodePointerUp handler =
    Json.Decode.map handler
        (Json.Decode.field "pointerId" Json.Decode.value)


decodePointerMove : (Point2d Pixels CanvasCoordinates -> msg) -> Json.Decode.Decoder msg
decodePointerMove handler =
    Json.Decode.map2 (\x y -> handler (Point2d.pixels x y))
        (Json.Decode.at [ "clientX" ] Json.Decode.float)
        (Json.Decode.at [ "clientY" ] Json.Decode.float)


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.map2
        (\key ctrlKey ->
            UserPressedKeyDown key
                { ctrlKey = ctrlKey }
        )
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)


getShapeInstanceAtTime : Float -> NonemptyArray ShapeInstance -> Maybe ( ShapeInstance, Int )
getShapeInstanceAtTime offset timeline =
    timeline
        |> Nonempty.Array.toList
        |> getShapeInstanceAtTimeHelper 0 offset


getShapeInstanceAtTimeHelper : Int -> Float -> List ShapeInstance -> Maybe ( ShapeInstance, Int )
getShapeInstanceAtTimeHelper index offset timelines =
    case timelines of
        [] ->
            Nothing

        next :: rest ->
            let
                nextOffset =
                    offset - next.offset
            in
            if abs nextOffset <= 0.001 then
                Just ( next, index )

            else
                getShapeInstanceAtTimeHelper (index + 1) nextOffset rest


getShapeAtTime : Float -> NonemptyArray ShapeInstance -> Maybe Shape
getShapeAtTime offset timeline =
    timeline
        |> Nonempty.Array.uncons
        |> Tuple.mapSecond Array.toList
        |> getShapeAtTimeHelper 0 offset


getShapeAtTimeHelper : Int -> Float -> ( ShapeInstance, List ShapeInstance ) -> Maybe Shape
getShapeAtTimeHelper index offset timeline =
    case timeline of
        ( first, [] ) ->
            if offset < first.offset then
                Nothing

            else
                Just first.shape

        ( first, next :: rest ) ->
            if offset < first.offset then
                Nothing

            else
                let
                    remainingOffset =
                        offset - first.offset
                in
                if remainingOffset <= next.offset then
                    let
                        offsetDistance =
                            remainingOffset / next.offset
                    in
                    interpolateShapeFrom first.shape next.shape offsetDistance

                else
                    getShapeAtTimeHelper (index + 1) remainingOffset ( next, rest )


type FindKeyedShape
    = KeyedShapeAlreadyAtTime
    | InterpolatedShape { shape : Shape, offset : Float, index : Int }
    | NoShapeFound


getShapeAfterTime : Float -> NonemptyArray ShapeInstance -> FindKeyedShape
getShapeAfterTime offset timeline =
    timeline
        |> Nonempty.Array.uncons
        |> Tuple.mapSecond Array.toList
        |> getShapeAfterTimeHelper 0 offset


getShapeAfterTimeHelper : Int -> Float -> ( ShapeInstance, List ShapeInstance ) -> FindKeyedShape
getShapeAfterTimeHelper index offset timeline =
    case timeline of
        ( first, [] ) ->
            let
                nextOffset =
                    offset - first.offset
            in
            if abs nextOffset <= 0.001 then
                KeyedShapeAlreadyAtTime

            else if offset < first.offset then
                InterpolatedShape
                    { shape = first.shape
                    , offset = offset
                    , index = index
                    }

            else
                NoShapeFound

        ( first, next :: rest ) ->
            let
                nextOffset =
                    offset - first.offset
            in
            if abs nextOffset <= 0.001 then
                KeyedShapeAlreadyAtTime

            else if offset < first.offset then
                NoShapeFound

            else
                let
                    remainingOffset =
                        offset - first.offset
                in
                if abs remainingOffset <= 0.001 then
                    KeyedShapeAlreadyAtTime

                else if abs (remainingOffset - next.offset) <= 0.001 then
                    KeyedShapeAlreadyAtTime

                else if remainingOffset < next.offset then
                    let
                        offsetDistance =
                            remainingOffset / next.offset
                    in
                    case interpolateShapeFrom first.shape next.shape offsetDistance of
                        Nothing ->
                            NoShapeFound

                        Just shape ->
                            InterpolatedShape
                                { shape = shape
                                , offset = remainingOffset
                                , index = index
                                }

                else
                    getShapeAfterTimeHelper (index + 1) remainingOffset ( next, rest )


interpolateShapeFrom : Shape -> Shape -> Float -> Maybe Shape
interpolateShapeFrom from to offset =
    case ( from, to ) of
        ( LineSegment fromLine, LineSegment toLine ) ->
            Just
                (LineSegment
                    { p1 = Point2d.interpolateFrom fromLine.p1 toLine.p1 offset
                    , p2 = Point2d.interpolateFrom fromLine.p2 toLine.p2 offset
                    , color = Util.Color.interpolateFrom fromLine.color toLine.color offset
                    }
                )

        _ ->
            Nothing
