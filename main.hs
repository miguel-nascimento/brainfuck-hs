import Data.Maybe ()
import Control.Applicative ( Alternative((<|>), empty) )
import GHC.Base

data Brainfuck =  MoveLeft        --  <
              | MoveRight         --  >
              | Increment         --  +
              | Decrement         --  -
              | Print             --  .
              | Input             --  ,
              | Loop [Brainfuck]  --  Commands inside a loop
              deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, input') <- p input
    Just (f x, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
                                (f, input')  <- p1 input
                                (a, input'') <- p2 input'
                                Just (f a, input'')
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser go where
  go (y:ys)
     | x == y    = Just (x, ys)
     | otherwise = Nothing
  go [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

bfLeft :: Parser Brainfuck
bfLeft = MoveLeft <$ charP '<'

bfRight :: Parser Brainfuck
bfRight = MoveRight <$ charP '>'

bfIncrement :: Parser Brainfuck 
bfIncrement = Increment <$ charP '+'

bfDecrement :: Parser Brainfuck
bfDecrement = Decrement <$ charP '-'

bfLoop :: Parser Brainfuck
bfLoop =  Loop <$> (charP '[' *> instructions <* charP ']')
  where instructions = many bfCommand

bfCommand :: Parser Brainfuck
bfCommand = bfLoop <|> bfDecrement<|> bfIncrement<|> bfLeft <|> bfRight 

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

