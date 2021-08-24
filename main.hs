import Data.Maybe 
import Control.Applicative
import GHC.Base

-- Data Structure
data Memory = Memory [Int] [Int]
    deriving Show
data Brainfuck =  MoveLeft        --  <
              | MoveRight         --  >
              | Increment         --  +
              | Decrement         --  -
              | Print             --  .
              | Input             --  ,
              | Loop [Brainfuck]  --  Commands inside a loop
              deriving (Show, Eq)

-- Parser

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

bfPrint :: Parser Brainfuck
bfPrint = Print <$ charP '.'

bfInput :: Parser Brainfuck
bfInput = Input <$ charP ','

bfCommand :: Parser Brainfuck
bfCommand = bfPrint <|> bfInput <|> bfLoop <|> bfDecrement
                <|> bfIncrement <|> bfLeft <|> bfRight 

-- NOTE: this is good? i'm asking how can i remove all the "Brainfuck" from the list
-- SOLUTION: combinate the parser with a until EOF parser and with a whitespace parser
bfParser :: Parser [Brainfuck]
bfParser = many bfCommand

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile filename parser = do 
  input <- readFile filename
  return $ fst <$> runParser parser input


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
