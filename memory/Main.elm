module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = createModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Model
    = Playing Deck


type alias Card =
    { id : String
    , group : Group
    , flipped : Bool
    }


type alias Deck =
    List Card


type Group
    = A
    | B


type Msg
    = NoOp


cards : List String
cards =
    [ "dinosaur"
    , "8-ball"
    , "baked-potato"
    , "kronos"
    , "rocket"
    , "skinny-unicorn"
    , "that-guy"
    , "zeppelin"
    ]


createModel : ( Model, Cmd Msg )
createModel =
    ( Playing deck, Cmd.none )


initCard : Group -> String -> Card
initCard group name =
    { id = name
    , group = group
    , flipped = False
    }


deck : Deck
deck =
    let
        groupA =
            List.map (initCard A) cards

        groupB =
            List.map (initCard B) cards
    in
        List.concat [ groupA, groupB ]


randomList : (List Int -> Msg) -> Int -> Cmd Msg
randomList msg len =
    Random.int 0 100
        |> Random.list len
        |> Random.generate msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Playing deck ->
            div [ class "wrapper" ]
                (List.map createCard deck)


cardClass : Card -> String
cardClass card =
    "card-" ++ card.id


createCard : Card -> Html Msg
createCard card =
    div
        [ class "container"
        ]
        [ div [ classList [ ( "card", True ), ( "flipped", True ) ] ]
            [ div [ class "card-back" ] []
            , div [ class ("front " ++ cardClass card) ] []
            ]
        ]
