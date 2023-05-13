import Control.Monad.State
import Control.Monad.IO.Class
import Data.Time
import Distribution.Compat.Time (getCurTime)

logToFile :: String -> IO ()
logToFile message = appendFile "game_log.txt" (message ++ "\n")


-- | Type synonym for player actions
data Action = Cooperate | Cheat deriving (Eq, Show,Read)


-- | Type synonym for player scores
type Score = Int

listOfStrategy :: String
listOfStrategy = "Cheater | Cooperator* | CopyKitten* | Detective* | CopyCat | Grudger\n"

-- | Type for player strategies
data Strategy = Cheater | Cooperator | CopyKitten | Detective | CopyCat | Grudger | User deriving (Eq,Show,Read)

type GameLog = (Strategy, Action, Score, Strategy, Action, Score)

-- | Type for game state
data GameState = GameState
  { player1 :: (Strategy, Score)
  , player2 :: (Strategy, Score)
  , roundNum :: Int
  , log :: [GameLog]
  , actions_player1 :: [Action]
  , actions_player2 :: [Action]
  } deriving (Show)

getActionForUser :: IO Action
getActionForUser = do
  putStrLn "Write your action (Cooperate or Cheat):"
  readLn


getAction :: [Action] -> Strategy -> Int -> Int -> Action
getAction _ Cooperator _ _ = Cooperate
getAction _ Cheater _ _ = Cheat
getAction actions Grudger _ _ = 
  if isCheatInARow actions then
    Cheat
  else
    Cooperate
getAction actions CopyCat _ _ =
    if length actions == 0 then
        Cooperate
    else
        head (actions)
getAction actions CopyKitten _ _ =
    if isCheatTwiceInARow actions then
        Cheat
    else
        Cooperate
getAction actions Detective _ _ =
  if (length actions == 0 || length actions == 1 || length actions == 3) then
    Cooperate
  else if length actions == 1 then
    Cheat
  else if isCheatInARow actions then
    head (actions)
  else if isCheatInARow actions == False then
    Cheat
  else
    Cooperate


isCheatInARow :: [Action] -> Bool
isCheatInARow (x:xs) =
    if x == Cheat then
        True
    else
        isCheatInARow (xs)
isCheatInARow _ = False


isCheatTwiceInARow :: [Action] -> Bool
isCheatTwiceInARow (x:y:xs) =
    if x == Cheat && y == Cheat then
        True
    else
        isCheatTwiceInARow (y:xs)
isCheatTwiceInARow _ = False




calculateScore :: Action -> Action -> GameState -> GameState
calculateScore Cooperate Cooperate state@(GameState (s1, score1) (s2, score2) roundNum log actions_player1 actions_player2) =
  GameState (s1, score1 + 2) (s2, score2 + 2) (roundNum + 1)
    ((s1, Cooperate, score1 + 2, s2, Cooperate, score2 + 2) : log)
    (Cooperate : actions_player1)
    (Cooperate : actions_player2)
    

calculateScore Cheat Cooperate state@(GameState (s1, score1) (s2, score2) roundNum log actions_player1 actions_player2) =
  GameState (s1, score1 + 3) (s2, score2-1) (roundNum + 1)
    ((s1, Cheat, score1 + 3, s2, Cooperate, score2-1) : log)
    (Cheat : actions_player1)
    (Cooperate : actions_player2)

calculateScore Cooperate Cheat state@(GameState (s1, score1) (s2, score2) roundNum log actions_player1 actions_player2) =
  GameState (s1, score1-1) (s2, score2 + 3) (roundNum + 1)
    ((s1, Cooperate, score1-1, s2, Cheat, score2 + 3) : log)
    (Cooperate : actions_player1)
    (Cheat : actions_player2)

calculateScore Cheat Cheat state@(GameState (s1, score1) (s2, score2) roundNum log actions_player1 actions_player2) =
  GameState (s1, score1) (s2, score2) (roundNum + 1)
    ((s1, Cheat, score1, s2, Cheat, score2) : log)
    (Cheat : actions_player1)
    (Cheat : actions_player2)

playRound :: (MonadIO m, MonadState GameState m) => m ()
playRound = do
    -- получаем текущее состояние игры
    state <- get

    -- определяем выбор игроков на этом ходу

    let strategy1 = fst (player1 state)
    let score1 = snd (player1 state)
    let strategy2 = fst (player2 state)
    let score2 = snd (player2 state) 
    
    action1 <- if strategy1 /= User
           then pure $ getAction (actions_player2 state) strategy1 score1 score2
           else liftIO $ getActionForUser
           
    let action2 = getAction (actions_player1 state) strategy2 score2 score1

    -- обновляем счет игры
    let newState = calculateScore action1 action2 state

    -- записываем результаты раунда в лог игры
    
    liftIO $ if (strategy1 == User) 
          then putStrLn $ "You: " ++ show (snd (player1 newState)) ++ "\nOpponent: " ++ show (snd (player2 newState))
          else return ()
    let logString = show (roundNum newState) ++ ": " ++ show (player1 newState) ++ " " ++ show action1 ++ "\t" ++ show action2 ++ " " ++ show (player2 newState) ++ " "
    liftIO $ logToFile logString
    
    put newState


main :: IO ()
main = do

  putStrLn $ "Type `simulate` or `game` mode"
  game_mode <- getLine
  putStrLn $ "Your game mode is:\t " ++ game_mode ++ "\n"
  if game_mode == "simulate" then do
    putStr $ "Input strategy of bot 1: " ++ listOfStrategy
    strategy1 <- readLn
    putStr $ "Input strategy of bot 2: "++ listOfStrategy
    strategy2 <- readLn

    let zeroState = GameState (strategy1, 0) (strategy2, 0) 0 [] [] []
    putStr $ "Input count of rounds:\n "
    numRounds <- readLn -- count of rounds
    putStrLn $ "Game Started\nPlayer1\t"++"\t" ++ "Player2\n" ++ show strategy1 ++ "\t" ++ show strategy2 ++ "\n"
    let curLog = "-------Game session-------\n"
    logToFile curLog
    -- play the specified number of rounds
    finalState <- execStateT (replicateM numRounds playRound) zeroState

    -- print the final scores
    putStrLn $ "Final Scores:\nPlayer1: " ++ show (snd $ player1 finalState) ++ "\nPlayer2: " ++ show (snd $ player2 finalState)
  else do
    putStrLn $ "Input strategy of opponent: " ++ listOfStrategy
    strategy1 <- readLn

    let zeroState = GameState (User, 0) (strategy1, 0) 0 [] [] []
    putStrLn $ "Input count of rounds:\n "
    numRounds <- readLn -- count of rounds
    

    putStrLn $ "Game Started\nPlayer1\t"++"\t" ++ "Player2\n" ++ show User ++ "\t" ++ show strategy1 ++ "\n"
    
    let curLog = "-------Game session-------\n"
    logToFile curLog
    -- play the specified number of rounds
    finalState <- execStateT (replicateM numRounds playRound) zeroState

    -- print the final scores
    putStrLn $ "Final Scores:\nPlayer1: " ++ show (snd $ player1 finalState) ++ "\nPlayer2: " ++ show (snd $ player2 finalState)

  
