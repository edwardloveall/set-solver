module Main exposing (main)

import Browser
import Card
    exposing
        ( Card
        , ColorAttribute(..)
        , CountAttribute(..)
        , FillAttribute(..)
        , ShapeAttribute(..)
        , cardNeededForSet
        , displayedCard
        )
import Html exposing (Html, div)



-- APP


main : Program () Model msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = \_ -> \model -> model
        }



-- MODEL


type alias Model =
    List Card


card1 : Card
card1 =
    Card Single Red Empty Diamond


card2 : Card
card2 =
    Card Double Green Striped Oval


initialModel : Model
initialModel =
    [ card1, card2 ]



-- VIEW


view : Model -> Html msg
view _ =
    div []
        (List.map
            displayedCard
            [ card1, card2, cardNeededForSet card1 card2 ]
        )
