module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Model
    = PlayerTurn Game
    | CompTurn Game
    | CheckResult Game
    | CheckWin Game
    | PlayerWon Game
    | ComputerWon Game


type alias Game =
    { deck : Deck
    , card1 : Maybe Card
    , card2 : Maybe Card
    , playerScore : Score
    , compScore : Score
    }


type alias Deck =
    List Card


type alias Score =
    Int


type alias Card =
    { rank : Int
    , suit : String
    , flipped : Bool
    }


type Msg
    = ClickDeck Deck
    | Reset


deck : List Card
deck =
    let
        suits =
            [ "H", "C", "D", "S" ]

        ranks =
            List.range 2 10
    in
        List.map (\suit -> List.map (\rank -> (Card rank suit True)) ranks) suits
            |> List.foldr (++) []


initialModel : ( Model, Cmd Msg )
initialModel =
    PlayerTurn (Game deck Nothing Nothing 0 0)
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickDeck deck ->
            case model of
                PlayerTurn game ->
                    let
                        card =
                            List.head game.deck

                        newDeck =
                            List.drop 1 game.deck
                    in
                        CompTurn { game | deck = newDeck, card1 = card } ! []

                CompTurn game ->
                    let
                        card =
                            List.head game.deck

                        newDeck =
                            List.drop 1 game.deck
                    in
                        CheckResult { game | deck = newDeck, card2 = card } ! []

                CheckResult game ->
                    let
                        score1 =
                            case game.card1 of
                                Just card ->
                                    card.rank

                                Nothing ->
                                    0

                        score2 =
                            case game.card2 of
                                Just card ->
                                    card.rank

                                Nothing ->
                                    0
                    in
                        if score1 > score2 then
                            CheckWin { game | playerScore = game.playerScore + 1 } ! []
                        else
                            CheckWin { game | compScore = game.compScore + 1 } ! []

                CheckWin game ->
                    if game.playerScore >= 5 then
                        PlayerWon game ! []
                    else if game.compScore >= 5 then
                        ComputerWon game ! []
                    else
                        PlayerTurn game ! []

                PlayerWon game ->
                    PlayerWon game ! []

                ComputerWon game ->
                    ComputerWon game ! []

        Reset ->
            initialModel


cardView : Card -> Html Msg
cardView card =
    let
        source =
            if card.flipped then
                (toString card.rank) ++ card.suit ++ ".svg"
            else
                "Red_Back.svg"
    in
        img
            [ class "card"
            , src ("cards/" ++ source)
            ]
            []


deckView : List Card -> Html Msg
deckView deck =
    img
        [ class "deck"
        , src "cards/Red_Back.svg"
        , onClick (ClickDeck deck)
        ]
        []


tableView : Maybe Card -> Maybe Card -> Html Msg
tableView card1 card2 =
    let
        card1View =
            case card1 of
                Just card ->
                    cardView card

                Nothing ->
                    span [] []

        card2View =
            case card2 of
                Just card ->
                    cardView card

                Nothing ->
                    span [] []
    in
        div []
            [ card1View
            , card2View
            ]


scoreView : String -> Int -> Html Msg
scoreView prefix score =
    div []
        [ text (prefix ++ ": " ++ (toString score))
        ]


gameOverView : String -> Int -> Html Msg
gameOverView prefix score =
    div []
        [ p [] [ text (prefix ++ " won with " ++ toString score ++ " points!") ]
        , text "Do you want to "
        , span [ onClick Reset ] [ text "play again?" ]
        ]


gameView : Game -> Html Msg
gameView game =
    div []
        [ scoreView "Player" game.playerScore
        , scoreView "Computer" game.compScore
        , deckView game.deck
        , tableView game.card1 game.card2
        ]


view : Model -> Html Msg
view model =
    case model of
        PlayerTurn game ->
            gameView game

        CompTurn game ->
            gameView game

        CheckResult game ->
            gameView game

        CheckWin game ->
            gameView game

        PlayerWon game ->
            gameOverView "Player" game.playerScore

        ComputerWon game ->
            gameOverView "Computer" game.compScore
