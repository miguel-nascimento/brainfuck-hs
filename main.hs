import Data.Maybe ( fromJust )
import GHC.Base ( Alternative(..), ord )
import Data.Char
import System.Environment ( getArgs )
import Control.Applicative ( Alternative(..) )

-- Data Structure
data Memory = Memory [Int] [Int]
    deriving (Show)

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

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
    in Just (token, rest)

ignore :: Parser [Char]
ignore = spanP isSpace 

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
  where instructions = some bfCommand

bfPrint :: Parser Brainfuck
bfPrint = Print <$ charP '.'

bfInput :: Parser Brainfuck
bfInput = Input <$ charP ','

bfCommand :: Parser Brainfuck
bfCommand = ignore *> (bfPrint <|> bfInput <|> bfLoop <|> bfDecrement
                <|> bfIncrement <|> bfLeft <|> bfRight) <* ignore

bfParser :: Parser [Brainfuck]
bfParser = ignore *> many bfCommand <* ignore 

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

run :: [Brainfuck] -> IO Memory
run = go emptyMemory
  where go memory []     = return memory
        go memory (x:xs) = case x of
          MoveRight  -> go (moveRight                 memory) xs
          MoveLeft   -> go (moveLeft                  memory) xs
          Increment  -> go (modifyMemory (+1)         memory) xs
          Decrement  -> go (modifyMemory (subtract 1) memory) xs
          Input      -> do
            c <- getChar
            go (modifyMemory (const $ ord c) memory) xs
          Print     -> do
            putChar (chr (readCell memory))
            go memory xs
          Loop cmds -> if readCell memory /= 0
                       then do
                         memory' <- go memory cmds
                         go memory' (x:xs)
                       else go memory xs

main :: IO Memory
main = do 
  filepath <-  getArgs 
  cmds <- readFile $ head filepath
  run $ fromJust $ fst <$> runParser bfParser cmds
