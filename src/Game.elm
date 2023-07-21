module Game exposing (..)


type GameAction
    = Rock
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
                Rock ->
                    Tie

                Scissors ->
                    Win

                Paper ->
                    Loss

        Paper ->
            case computerMove of
                Paper ->
                    Tie

                Rock ->
                    Win

                Scissors ->
                    Loss

        Scissors ->
            case computerMove of
                Scissors ->
                    Tie

                Paper ->
                    Win

                Rock ->
                    Loss


type alias Player =
    { name : String
    , lastMove : GameAction
    , wins : Int
    }


type alias GameState =
    { user : Player
    , computer : Player
    , lastRound : RoundResult
    , ties : Int
    , totalRounds : Int
    }


freshPlayer : String -> Player
freshPlayer name =
    Player name Rock 0


freshState : GameState
freshState =
    GameState (freshPlayer "You") (freshPlayer "Computer") Tie 0 0


doUserMove : GameState -> GameAction -> GameState
doUserMove game move =
    let
        gameUser =
            game.user
    in
    { game | user = { gameUser | lastMove = move } }


judgeRound : GameState -> GameState
judgeRound model =
    let
        userMove =
            model.user.lastMove

        compMove =
            model.computer.lastMove
    in
    { model | lastRound = compareMoves userMove compMove }


incrementWins : Player -> Player
incrementWins player =
    { player | wins = player.wins + 1 }


setComputerMove : GameState -> GameAction -> GameState
setComputerMove game move =
    let
        gameComputer =
            game.computer
    in
    { game | computer = { gameComputer | lastMove = move } }


updateScores : GameState -> GameState
updateScores game =
    case game.lastRound of
        Tie ->
            { game | ties = game.ties + 1 }

        Win ->
            { game | user = incrementWins game.user }

        Loss ->
            { game | computer = incrementWins game.computer }


doComputerMove : GameState -> GameAction -> GameState
doComputerMove game move =
    setComputerMove game move
        |> judgeRound
        |> updateScores
        |> (\model -> { model | totalRounds = model.totalRounds + 1 })
