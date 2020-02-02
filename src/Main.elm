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
import Html exposing (Html, div, h2, p, section, text)
import Html.Attributes exposing (class)
import List.Extra exposing (uniquePairs)
import Random
import Random.List exposing (shuffle)



-- APP


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    List Card


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( [], Random.generate NewBoard (shuffle allCards) )


allCards : List Card
allCards =
    List.Extra.lift4
        Card
        [ Single, Double, Triple ]
        [ Red, Green, Purple ]
        [ Empty, Striped, Filled ]
        [ Diamond, Oval, Squiggle ]



-- UPDATE


type Msg
    = NewBoard (List Card)


update : Msg -> Model -> ( Model, Cmd msg )
update msg _ =
    case msg of
        NewBoard cards ->
            ( List.take 12 cards, Cmd.none )



-- VIEW


view : Model -> Html msg
view model =
    div [ class "wrapper" ]
        [ h2 [] [ text "board" ]
        , section
            [ class "board" ]
            (List.map
                displayedCard
                model
            )
        , h2 [] [ text "solutions" ]
        , List.map (List.map displayedCard) (solutions model) |> List.map (p []) |> section [ class "solutions" ]
        ]


solutions : Model -> List (List Card)
solutions cards =
    List.map (fullMatch cards) (uniquePairs cards)


fullMatch : Model -> ( Card, Card ) -> List Card
fullMatch cards ( card1, card2 ) =
    let
        match =
            cardNeededForSet card1 card2
    in
    if matchExistsOnBoard cards match then
        [ card1, card2, match ]

    else
        []


matchExistsOnBoard : Model -> Card -> Bool
matchExistsOnBoard cards match =
    List.member match cards
