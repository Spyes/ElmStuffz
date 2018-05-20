module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    | Guessing Deck Card
    | MatchCard Deck Card Card
    | GameOver Deck


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
    = Flip Card
    | Shuffle (List Int)
    | Reset
    | NoOp


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


deck : Deck
deck =
    let
        groupA =
            List.map (initCard A) cards

        groupB =
            List.map (initCard B) cards
    in
        List.concat [ groupA, groupB ]


createModel : ( Model, Cmd Msg )
createModel =
    let
        model =
            Playing deck

        cmd =
            randomList Shuffle (List.length deck)
    in
        ( model, cmd )


initCard : Group -> String -> Card
initCard group name =
    { id = name
    , group = group
    , flipped = False
    }


randomList : (List Int -> Msg) -> Int -> Cmd Msg
randomList msg len =
    Random.int 0 100
        |> Random.list len
        |> Random.generate msg


shuffleDeck : Deck -> List comparable -> Deck
shuffleDeck deck xs =
    List.map2 (,) deck xs
        |> List.sortBy Tuple.second
        |> List.unzip
        |> Tuple.first


flip : Bool -> Card -> Card -> Card
flip isFlipped a b =
    if (a.id == b.id) && (a.group == b.group) then
        { b | flipped = isFlipped }
    else
        b


checkIfCorrect : Card -> Model -> ( Model, Cmd Msg )
checkIfCorrect card model =
    case model of
        Playing deck ->
            let
                newDeck =
                    List.map (flip True card) deck
            in
                Guessing newDeck card ! []

        Guessing deck guess ->
            let
                newDeck =
                    List.map (flip True card) deck

                isOver =
                    List.all .flipped newDeck

                newModel =
                    if isOver then
                        GameOver newDeck
                    else
                        MatchCard newDeck guess card
            in
                newModel ! []

        MatchCard deck guess1 guess2 ->
            if guess1.id == guess2.id then
                update (Flip card) (Playing deck)
            else
                let
                    flipGuess =
                        flip False guess1 >> flip False guess2

                    newDeck =
                        List.map flipGuess deck
                in
                    Playing newDeck ! []

        GameOver deck ->
            GameOver deck ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle xs ->
            let
                newDeck =
                    shuffleDeck deck xs
            in
                Playing newDeck ! []

        Flip card ->
            if card.flipped then
                model ! []
            else
                checkIfCorrect card model

        Reset ->
            createModel

        NoOp ->
            model ! []


wrapper : Deck -> Html Msg -> Html Msg
wrapper deck overlay =
    div [ class "wrapper" ]
        [ div [] (List.map createCard deck)
        , overlay
        ]


game : Deck -> Html Msg
game deck =
    wrapper deck (text "")


view : Model -> Html Msg
view model =
    case model of
        Playing deck ->
            game deck

        Guessing deck _ ->
            game deck

        MatchCard deck _ _ ->
            game deck

        GameOver deck ->
            wrapper deck playAgainOverlay


playAgainOverlay : Html Msg
playAgainOverlay =
    div [ class "congrats" ]
        [ p [] [ text "Yay! You won!" ]
        , text "Do you want to "
        , span [ onClick Reset ] [ text "play again?" ]
        ]


cardClass : Card -> String
cardClass card =
    "card-" ++ card.id


createCard : Card -> Html Msg
createCard card =
    div
        [ class "container"
        ]
        [ div
            [ classList [ ( "card", True ), ( "flipped", card.flipped ) ]
            , onClick (Flip card)
            ]
            [ div [ class "card-back" ] []
            , div [ class ("front " ++ cardClass card) ] []
            ]
        ]
