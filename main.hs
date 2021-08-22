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


newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

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

bfSource :: String
bfSource = "+>++>+++>[++++]<-"

