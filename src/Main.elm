module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type GameAction
    =  Rock
    | Paper
    | Scissors

type RoundResult
    = Win
    | Tie
    | Loss


compareMoves : GameAction -> GameAction -> RoundResult
compareMoves userMove computerMove =
    case userMove of
        Rock ->
            case computerMove of
                Rock -> Tie
                Scissors -> Win
                Paper -> Loss
        Paper ->
            case computerMove of
                Paper -> Tie
                Rock -> Win
                Scissors -> Loss
        Scissors ->
            case computerMove of
                Scissors -> Tie
                Paper -> Win
                Rock -> Loss


type alias Player =
    { lastMove : GameAction
    , wins: Int
    }

type alias Model =
    { user: Player
    , computer: Player
    , ties: Int
    , totalRounds: Int
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( Model (Player Rock 0) (Player Rock 0) 0 0
    , Cmd.none
    )


-- Some helpful constants to keep around
freshPlayer = Player Rock 0
freshState = Model freshPlayer freshPlayer 0 0


doUserMove : Model -> GameAction -> Model
doUserMove game move =
    let
        gameUser = game.user
    in
        { game | user = { gameUser | lastMove = move } }



compareMovesInGame : Model -> RoundResult
compareMovesInGame model =
    let
        userMove = model.user.lastMove
        compMove = model.computer.lastMove
    in
        compareMoves userMove compMove
 

incrementWins: Player -> Player
incrementWins player =
    { player | wins = player.wins + 1 }


setComputerMove : Model -> GameAction -> Model
setComputerMove game move =
    let
        gameComputer = game.computer
    in
        { game | computer = { gameComputer | lastMove = move } }


scoreRound : Model -> Model
scoreRound game =
    case (compareMovesInGame game) of
        Tie ->
            { game | ties = game.ties + 1 }
        Win ->
            { game | user = ( incrementWins game.user ) }
        Loss ->
            { game | computer = ( incrementWins game.computer ) } 


doComputerMove : Model -> GameAction -> Model
doComputerMove game move =
    setComputerMove game move
        |> scoreRound
      

type Msg
    = Reset               -- Reset the game to initial state
    | UserPlay GameAction -- Set the player's move & request a computer move
    | CompPlay GameAction -- Set the computer move & score the response


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        Reset ->
            ((freshState), Cmd.none)

        UserPlay move ->
            ( (doUserMove model move)
            , Random.generate CompPlay (Random.uniform Rock [ Paper, Scissors ])
            )

        CompPlay move ->
            ( (doComputerMove model move)
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewAction : GameAction -> String
viewAction action =
    case action of
        Rock -> "Rock"
        Paper -> "Paper"
        Scissors -> "Scissors"


viewPlayerWins : Player -> String
viewPlayerWins player =
    String.fromInt player.wins


viewTies : Model -> String
viewTies game =
    String.fromInt game.ties


view : Model -> Html Msg
view game =
    div []

        [
            h1 [] [ text "Rock, Paper, Scissors in Elm!" ]
            , table []
                    [ tr [] [ td [] [ text "User" ]
                            , td [] [ text (viewPlayerWins game.user) ]
                            ]
                    , tr [] [ td [] [ text "Computer" ]
                            , td [] [ text (viewPlayerWins game.computer) ]
                            ]
                    , tr [] [ td [] [ text "Ties" ]
                            , td [] [ text (viewTies game) ] 
                            ]
                    ]
            , button [ onClick (UserPlay Rock) ] [ text "Rock" ]
            , button [ onClick (UserPlay Paper) ] [ text "Paper" ]
            , button [ onClick (UserPlay Scissors) ] [ text "Scissors" ]
            , button [ onClick Reset ] [ text "Reset Game" ]
        ]

