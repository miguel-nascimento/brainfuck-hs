import Data.Maybe

data Command =  MoveLeft      -- <
              | MoveRight     -- >
              | Increment     -- +
              | Decrement     -- -
              | Print         -- .
              | TakeInput     -- ,
              | LoopL         -- [
              | LoopR         -- ]
              deriving Show


charToCommand :: Char -> Maybe Command
charToCommand s = case s of
            '<' -> Just MoveLeft
            '>' -> Just MoveLeft
            '+' -> Just Increment
            '-' -> Just Decrement
            '.' -> Just Print
            ',' -> Just TakeInput
            '[' -> Just LoopL
            ']' -> Just LoopR
            _   -> Nothing

parseCommand :: String -> [Command]
parseCommand = mapMaybe charToCommand

data Tape a = Tape [a] Int [a]
    deriving Show

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
  where zeros = repeat 0

moveRight :: Tape Int -> Tape Int
moveRight (Tape ls x (r:rs)) = Tape (x:ls) r rs


moveLeft :: Tape Int -> Tape Int
moveLeft (Tape (l:ls) x rs) = Tape ls l (x:rs)

printCell :: Tape Int -> String
printCell (Tape _ a _) = show a

perform :: Command -> Tape Int -> Tape Int
perform command tape
  | command == MoveLeft  = moveLeft tape
  | command == MoveRight = moveRight tape
  | command == Increment = increment cell
  | command == Decrement = decrement cell
  | command == Print     = printCell cell
  | command == TakeInput = takeInput tape
  | command == LoopL     = _
  | command == LoopR     = _
  | otherwise            = _


run :: [Command] -> IO()
run = undefined 