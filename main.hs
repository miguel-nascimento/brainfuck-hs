import Data.Maybe
import Data.Char
import GHC.Base

data Command =  MoveLeft          --  <
              | MoveRight         --  >
              | Increment         --  +
              | Decrement         --  -
              | Print             --  .
              | Input             --  ,
              | LoopL             --  [
              | LoopR             --  ]
              | Loop [Command]    -- commands inside a loop
              deriving Show


charToCommand :: Char -> Maybe Command
charToCommand s = case s of
            '<' -> Just MoveRight
            '>' -> Just MoveLeft
            '+' -> Just Increment
            '-' -> Just Decrement
            '.' -> Just Print
            ',' -> Just Input
            '[' -> Just LoopL
            ']' -> Just LoopR
            _   -> Nothing

parseCommand :: String -> [Command]
parseCommand = mapMaybe charToCommand

data Memory = Memory [Int] [Int]
    deriving Show

emptyMemory :: Memory
emptyMemory = Memory [] []

moveRight :: Memory -> Memory
moveRight (Memory ls (r:rs)) = Memory (r:ls) rs
moveRight (Memory ls []) = Memory (0:ls) []


moveLeft :: Memory -> Memory
moveLeft (Memory (l:ls) rs) = Memory ls (l:rs)
moveLeft (Memory [] rs) = Memory [] (0:rs)

modifyMemory :: (Int -> Int) -> Memory -> Memory
modifyMemory f (Memory left (x:rs)) = Memory left (f x:rs)
modifyMemory f (Memory left []) = Memory left [f 0]

readCell :: Memory -> Int
readCell (Memory _ (x:rs)) = x
readCell (Memory _ [])     = 0


exec :: Memory -> [Command] -> IO Memory
exec memory []     = returnIO memory
exec memory (x:xs) = case x of
  MoveLeft  -> exec (moveLeft memory) xs
  MoveRight -> exec (moveRight memory) xs
  Increment -> exec (modifyMemory (+1) memory) xs
  Decrement -> exec (modifyMemory (\x -> x - 1) memory) xs
  Print -> do 
    putChar (chr $ readCell memory)
    exec memory xs
  Input -> do
    c <- getChar 
    exec (modifyMemory ( const $ ord c) memory) xs 
  _ -> undefined 

bfSource :: [Char]
bfSource = "+>++>+++>[++++]<-"

