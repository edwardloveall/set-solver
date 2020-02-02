module Main exposing (main)

import Browser
import Card
    exposing
        ( Card
        , ColorAttribute(..)
        , CountAttribute(..)
        , FillAttribute(..)
        , ShapeAttribute(..)
        , displayedCard
        )
import Html exposing (Html, section)
import Html.Attributes exposing (class)
import List.Extra
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



-- UPDATE


type Msg
    = NewBoard (List Card)


update : Msg -> Model -> ( Model, Cmd msg )
update msg _ =
    case msg of
        NewBoard cards ->
            ( cards, Cmd.none )



-- VIEW


view : Model -> Html msg
view model =
    section [ class "wrapper" ]
        (List.map
            displayedCard
            (randomBoard model)
        )


randomBoard : Model -> List Card
randomBoard model =
    List.take 12 model


allCards : List Card
allCards =
    List.Extra.lift4
        Card
        [ Single, Double, Triple ]
        [ Red, Green, Purple ]
        [ Empty, Striped, Filled ]
        [ Diamond, Oval, Squiggle ]
