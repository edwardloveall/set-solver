module Card exposing
    ( Card
    , ColorAttribute(..)
    , CountAttribute(..)
    , FillAttribute(..)
    , ShapeAttribute(..)
    , cardNeededForSet
    , cardToString
    , displayedCard
    )

import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class)


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


cardAttributes : Card -> List String
cardAttributes card =
    [ countToString card.count
    , colorToString card.color
    , fillToString card.fill
    , shapeToString card.shape
    ]


cardText : Card -> Html msg
cardText card =
    let
        characters =
            case card.shape of
                Diamond ->
                    List.repeat (countToInt card.count) "♦︎"

                Oval ->
                    List.repeat (countToInt card.count) "⬮"

                Squiggle ->
                    List.repeat (countToInt card.count) "∿"
    in
    String.join "" characters |> text


countToInt : CountAttribute -> Int
countToInt count =
    case count of
        Single ->
            1

        Double ->
            2

        Triple ->
            3


displayedCard : Card -> Html msg
displayedCard card =
    div (cardClasses card) [ cardText card ]


cardClasses : Card -> List (Attribute msg)
cardClasses card =
    List.map String.toLower (cardAttributes card)
        |> List.map class
        |> List.append [ class "card" ]


cardToString : Card -> String
cardToString card =
    String.join " " (cardAttributes card)


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
