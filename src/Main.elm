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


viewRoundResult result =
    case result of
        Win -> "You win the round!"
        Tie -> "It's a tie!"
        Loss -> "You lose the round!"


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
    { name: String
    , lastMove : GameAction
    , wins: Int
    }

type alias Model =
    { user: Player
    , computer: Player
    , lastRound: RoundResult
    , ties: Int
    , totalRounds: Int
    }


-- Some helpful default functions
freshPlayer : String -> Player
freshPlayer name =
    Player name Rock 0
freshState : Model
freshState = Model (freshPlayer "You") (freshPlayer "Computer") Tie 0 0


init : () -> (Model, Cmd Msg)
init _ =
    ( (freshState)
    , Cmd.none
    )


doUserMove : Model -> GameAction -> Model
doUserMove game move =
    let
        gameUser = game.user
    in
        { game | user = { gameUser | lastMove = move } }



judgeRound : Model -> Model
judgeRound model =
    let
        userMove = model.user.lastMove
        compMove = model.computer.lastMove
    in
        { model | lastRound = (compareMoves userMove compMove) }
 

incrementWins: Player -> Player
incrementWins player =
    { player | wins = player.wins + 1 }


setComputerMove : Model -> GameAction -> Model
setComputerMove game move =
    let
        gameComputer = game.computer
    in
        { game | computer = { gameComputer | lastMove = move } }


updateScores : Model -> Model
updateScores game =
    case game.lastRound of
        Tie ->
            { game | ties = game.ties + 1 }
        Win ->
            { game | user = ( incrementWins game.user ) }
        Loss ->
            { game | computer = ( incrementWins game.computer ) }


doComputerMove : Model -> GameAction -> Model
doComputerMove game move =
    setComputerMove game move
        |> judgeRound
        |> updateScores
        |> \model -> { model | totalRounds = model.totalRounds + 1 }
      

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

viewPlayerLastMove : Player -> String
viewPlayerLastMove player =
    viewAction player.lastMove

viewTies : Model -> String
viewTies game =
    String.fromInt game.ties


viewRoundResultDisplay : Model -> Html msg
viewRoundResultDisplay model =
    if model.totalRounds > 0 then
        div [] [ h2 [] [ text (String.join "" [ "You threw "
                                              , (viewPlayerLastMove model.user)
                                              , ", the computer threw "
                                              , (viewPlayerLastMove model.computer)
                                              , "."
                                              ]
                              )]
               , h2 [] [ text (viewRoundResult model.lastRound) ]
        ]
    else
        div [] [ p [] [ text "Choose a move to start"]]

viewStatRow : Player -> Html Msg
viewStatRow player =
   tr [] [ td [] [ text (player.name) ]
         , td [] [ text (String.fromInt player.wins) ]
   ]

view : Model -> Html Msg
view game =
    div []
        [
            h1 [] [ text "Rock, Paper, Scissors in Elm!" ]
            , (viewRoundResultDisplay game)
            , table []
                    [ caption [] [ text "Detailed Stats" ] 
                    , tr [] [ td [] [ text "" ]
                            , td [] [ text "Score" ]
                            ]
                    , (viewStatRow game.user)
                    , (viewStatRow game.computer)
                    ]
            , br [] []
            , button [ onClick (UserPlay Rock) ] [ text "Rock" ]
            , button [ onClick (UserPlay Paper) ] [ text "Paper" ]
            , button [ onClick (UserPlay Scissors) ] [ text "Scissors" ]
            , br [] []
            , br [] []
            , button [ onClick Reset ] [ text "Reset Game" ]
        ]
