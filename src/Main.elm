module Main exposing (main)

import Browser
import Html exposing (Html, div, p, span, text)



-- APP


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = \msg -> \model -> model
        }



-- MODEL


type alias Model =
    List Card


type CountAttribute
    = Single
    | Double
    | Triple


type ColorAttribute
    = Red
    | Green
    | Purple


type FillAttribute
    = Empty
    | Striped
    | Filled


type ShapeAttribute
    = Diamond
    | Oval
    | Squiggle


type alias Card =
    { count : CountAttribute
    , color : ColorAttribute
    , fill : FillAttribute
    , shape : ShapeAttribute
    }


card1 : Card
card1 =
    Card Single Red Empty Diamond


card2 : Card
card2 =
    Card Double Green Striped Oval


card3 : Card
card3 =
    Card Triple Purple Filled Squiggle


initialModel =
    [ card1, card2 ]


cardNeededForSet : Card -> Card -> Card
cardNeededForSet first second =
    Card
        (remainingCount first second)
        (remainingColor first second)
        (remainingFill first second)
        (remainingShape first second)


remainingCount : Card -> Card -> CountAttribute
remainingCount first second =
    case ( first.count, second.count ) of
        ( Single, Single ) ->
            Single

        ( Single, Double ) ->
            Triple

        ( Single, Triple ) ->
            Double

        ( Double, Single ) ->
            Triple

        ( Double, Double ) ->
            Double

        ( Double, Triple ) ->
            Single

        ( Triple, Single ) ->
            Double

        ( Triple, Double ) ->
            Single

        ( Triple, Triple ) ->
            Triple


remainingColor : Card -> Card -> ColorAttribute
remainingColor first second =
    case ( first.color, second.color ) of
        ( Red, Red ) ->
            Red

        ( Red, Green ) ->
            Purple

        ( Red, Purple ) ->
            Green

        ( Green, Red ) ->
            Purple

        ( Green, Green ) ->
            Green

        ( Green, Purple ) ->
            Red

        ( Purple, Red ) ->
            Green

        ( Purple, Green ) ->
            Red

        ( Purple, Purple ) ->
            Purple


remainingFill : Card -> Card -> FillAttribute
remainingFill first second =
    case ( first.fill, second.fill ) of
        ( Empty, Empty ) ->
            Empty

        ( Empty, Striped ) ->
            Filled

        ( Empty, Filled ) ->
            Striped

        ( Striped, Empty ) ->
            Filled

        ( Striped, Striped ) ->
            Striped

        ( Striped, Filled ) ->
            Empty

        ( Filled, Empty ) ->
            Striped

        ( Filled, Striped ) ->
            Empty

        ( Filled, Filled ) ->
            Filled


remainingShape : Card -> Card -> ShapeAttribute
remainingShape first second =
    case ( first.shape, second.shape ) of
        ( Diamond, Diamond ) ->
            Diamond

        ( Diamond, Oval ) ->
            Squiggle

        ( Diamond, Squiggle ) ->
            Oval

        ( Oval, Diamond ) ->
            Squiggle

        ( Oval, Oval ) ->
            Oval

        ( Oval, Squiggle ) ->
            Diamond

        ( Squiggle, Diamond ) ->
            Oval

        ( Squiggle, Oval ) ->
            Diamond

        ( Squiggle, Squiggle ) ->
            Squiggle


view : Model -> Html msg
view model =
    div []
        (List.map
            displayedCard
            [ card1, card2, cardNeededForSet card1 card2 ]
        )


displayedCard : Card -> Html msg
displayedCard card =
    p []
        [ text (countToString card.count ++ " ")
        , text (colorToString card.color ++ " ")
        , text (fillToString card.fill ++ " ")
        , text (shapeToString card.shape)
        ]


countToString : CountAttribute -> String
countToString count =
    case count of
        Single ->
            "Single"

        Double ->
            "Double"

        Triple ->
            "Triple"


colorToString : ColorAttribute -> String
colorToString color =
    case color of
        Red ->
            "Red"

        Green ->
            "Green"

        Purple ->
            "Purple"


fillToString : FillAttribute -> String
fillToString fill =
    case fill of
        Empty ->
            "Empty"

        Striped ->
            "Striped"

        Filled ->
            "Filled"


shapeToString : ShapeAttribute -> String
shapeToString shape =
    case shape of
        Diamond ->
            "Diamond"

        Oval ->
            "Oval"

        Squiggle ->
            "Squiggle"
