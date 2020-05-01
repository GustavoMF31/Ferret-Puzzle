module Main exposing (main)

import Array exposing (Array)
import Browser
import Element as El exposing (..)
import Element.Background
import Element.Border exposing (rounded)
import Element.Font
import Element.Input exposing (button)
import List.Extra exposing (last)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { boxes : List Bool
    , lastPositions : List (List Bool)
    , ferretSpeed : Int
    }


init : Model
init =
    { boxes = List.repeat 5 True
    , lastPositions = []
    , ferretSpeed = 1
    }



-- UPDATE


type Msg
    = BoxClicked Int
    | Reset
    | Undo
    | GameRuleChanged GameRuleChangedMsg


type GameRuleChangedMsg
    = MoreBoxes
    | LessBoxes
    | MoreFerretSpeed
    | LessFerretSpeed


indices : Array a -> List Int
indices array =
    Array.toIndexedList array |> List.map Tuple.first


beforeAfter : Int -> Int -> ( Int, Int )
beforeAfter n x =
    ( x - n, x + n )


ferretGameOf : Int -> Int -> Array Bool -> List Bool
ferretGameOf speed boxHitIndex boxes =
    let
        boxesAfterSmashing =
            Array.set boxHitIndex False boxes

        len =
            Array.length boxesAfterSmashing

        initialIndexList =
            boxesAfterSmashing
                |> indices
                |> List.map (beforeAfter speed)
                |> List.map tupleToList
    in
    initialIndexList
        |> mapHead (\h -> h ++ List.range 1 speed)
        |> mapLast (\l -> l ++ List.range (len - speed - 1) (len - 2))
        |> List.map (List.map (\i -> Array.get i boxesAfterSmashing))
        |> List.map maybeAny


mapHead : (a -> a) -> List a -> List a
mapHead f list =
    case list of
        [] ->
            []

        x :: xs ->
            f x :: xs


mapLast : (a -> a) -> List a -> List a
mapLast f list =
    case last list of
        Nothing ->
            []

        Just lastElement ->
            List.take (List.length list - 1) list ++ [ f lastElement ]


maybeAny : List (Maybe Bool) -> Bool
maybeAny list =
    list
        |> List.map (Maybe.withDefault False)
        |> List.any identity


tupleToList : ( a, a ) -> List a
tupleToList ( x, y ) =
    [ x, y ]


applyTuple : (a -> b -> c) -> ( a, b ) -> c
applyTuple func ( val1, val2 ) =
    func val1 val2


listWithTrues : Int -> List Bool
listWithTrues len =
    List.repeat len True


updateLastPositions : Model -> Model
updateLastPositions model =
    { model | lastPositions = model.boxes :: model.lastPositions }


resetLastPositions : Model -> Model
resetLastPositions model =
    { model | lastPositions = [] }


resetBoxes : Model -> Model
resetBoxes model =
    { model | boxes = listWithTrues (List.length model.boxes) }


update : Msg -> Model -> Model
update msg model =
    case msg of
        BoxClicked boxIndex ->
            { model
                | boxes = ferretGameOf model.ferretSpeed boxIndex (Array.fromList model.boxes)
            }
                |> updateLastPositions

        Reset ->
            resetBoxes model
                |> resetLastPositions

        Undo ->
            case model.lastPositions of
                [] ->
                    model

                x :: xs ->
                    { model | boxes = x, lastPositions = xs }

        GameRuleChanged gameRuleChangedMsg ->
            (case gameRuleChangedMsg of
                MoreBoxes ->
                    { model
                        | boxes = listWithTrues <| List.length model.boxes + 1
                    }

                LessBoxes ->
                    { model
                        | boxes = listWithTrues <| max (List.length model.boxes - 1) 1
                    }

                MoreFerretSpeed ->
                    { model
                        | ferretSpeed = model.ferretSpeed + 1
                    }

                LessFerretSpeed ->
                    { model
                        | ferretSpeed = max (model.ferretSpeed - 1) 1
                    }
            )
                |> resetBoxes
                |> resetLastPositions



-- VIEW


view model =
    let
        indexedTupleList =
            Array.toIndexedList (Array.fromList model.boxes)

        boxes =
            List.map (applyTuple viewBox) indexedTupleList
    in
    El.layout [] <|
        column [ centerX, centerY, spacing 10, width fill, padding 20 ]
            [ row [ spacing 20, width fill, height (px 200) ] boxes
            , row [ centerX, spacing 10 ]
                [ viewSimpleContainer "Quantidade de caixas:" (String.fromInt <| List.length model.boxes) viewMoreBoxesButton viewLessBoxesButton
                , viewSimpleContainer "Velocidade do furão:" (String.fromInt <| model.ferretSpeed) viewMoreFerretSpeedButton viewLessFerretSpeedButton
                , viewUndoButton
                , viewResetButton
                ]
            ]


viewSimpleContainer : String -> String -> Element Msg -> Element Msg -> Element Msg
viewSimpleContainer descriptionText currentValue firstButton secondButton =
    row
        [ spacing 10
        , padding 10

        -- , Element.Border.width 5
        -- , Element.Border.solid
        -- , Element.Border.color (rgb255 0 0 0)
        , Element.Border.rounded 5
        , Element.Background.color (rgb255 250 227 225)
        ]
        [ text descriptionText
        , text currentValue
        , column [] [ El.el [ centerX ] firstButton, El.el [ centerX ] secondButton ]
        ]


viewMoreBoxesButton : Element Msg
viewMoreBoxesButton =
    button [] { label = text "+", onPress = Just (GameRuleChanged MoreBoxes) }


viewLessBoxesButton : Element Msg
viewLessBoxesButton =
    button [] { label = text "-", onPress = Just (GameRuleChanged LessBoxes) }


viewMoreFerretSpeedButton : Element Msg
viewMoreFerretSpeedButton =
    button [] { label = text "+", onPress = Just (GameRuleChanged MoreFerretSpeed) }


viewLessFerretSpeedButton : Element Msg
viewLessFerretSpeedButton =
    button [] { label = text "-", onPress = Just (GameRuleChanged LessFerretSpeed) }


viewUndoButton : Element Msg
viewUndoButton =
    button [] { label = text "Undo", onPress = Just Undo }


viewResetButton : Element Msg
viewResetButton =
    button [] { label = text "Reset", onPress = Just Reset }


viewBox : Int -> Bool -> Element Msg
viewBox boxIndex hasFerret =
    Element.Input.button
        [ Element.Background.color (rgb255 245 176 66)
        , height fill
        , width fill
        , rounded 5
        , Element.Font.size 90
        ]
        { label =
            el [ centerX, centerY ]
                (text
                    (if hasFerret then
                        "F"

                     else
                        " "
                    )
                )
        , onPress = Just (BoxClicked boxIndex)
        }
