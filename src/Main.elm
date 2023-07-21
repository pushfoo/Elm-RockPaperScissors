module Main exposing (..)

import Browser
import Game exposing (..)
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


viewRoundResult result =
    case result of
        Win ->
            "You win the round!"

        Tie ->
            "It's a tie!"

        Loss ->
            "You lose the round!"


init : () -> ( GameState, Cmd Msg )
init _ =
    ( freshState
    , Cmd.none
    )


type Msg
    = Reset -- Reset the game to initial state
    | UserPlay GameAction -- Set the player's move & request a computer move
    | CompPlay GameAction -- Set the computer move & score the response


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( freshState, Cmd.none )

        UserPlay move ->
            ( doUserMove model move
            , Random.generate CompPlay (Random.uniform Rock [ Paper, Scissors ])
            )

        CompPlay move ->
            ( doComputerMove model move
            , Cmd.none
            )


subscriptions : GameState -> Sub Msg
subscriptions model =
    Sub.none


viewAction : GameAction -> String
viewAction action =
    case action of
        Rock ->
            "Rock"

        Paper ->
            "Paper"

        Scissors ->
            "Scissors"


viewPlayerWins : Player -> String
viewPlayerWins player =
    String.fromInt player.wins


viewPlayerLastMove : Player -> String
viewPlayerLastMove player =
    viewAction player.lastMove


viewTies : GameState -> String
viewTies game =
    String.fromInt game.ties


viewRoundResultDisplay : GameState -> Html msg
viewRoundResultDisplay model =
    if model.totalRounds > 0 then
        div []
            [ h2 []
                [ text
                    (String.join ""
                        [ "You threw "
                        , viewPlayerLastMove model.user
                        , ", the computer threw "
                        , viewPlayerLastMove model.computer
                        , "."
                        ]
                    )
                ]
            , h2 [] [ text (viewRoundResult model.lastRound) ]
            ]

    else
        div [] [ p [] [ text "Choose a move to start" ] ]


viewStatRow : Player -> Html Msg
viewStatRow player =
    tr []
        [ td [] [ text player.name ]
        , td [] [ text (String.fromInt player.wins) ]
        ]


view : GameState -> Html Msg
view game =
    div []
        [ h1 [] [ text "Rock, Paper, Scissors in Elm!" ]
        , viewRoundResultDisplay game
        , table []
            [ caption [] [ text "Detailed Stats" ]
            , tr []
                [ td [] [ text "" ]
                , td [] [ text "Score" ]
                ]
            , viewStatRow game.user
            , viewStatRow game.computer
            ]
        , br [] []
        , button [ onClick (UserPlay Rock) ] [ text "Rock" ]
        , button [ onClick (UserPlay Paper) ] [ text "Paper" ]
        , button [ onClick (UserPlay Scissors) ] [ text "Scissors" ]
        , br [] []
        , br [] []
        , button [ onClick Reset ] [ text "Reset Game" ]
        ]
