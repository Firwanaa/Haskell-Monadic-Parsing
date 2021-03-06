import Control.Applicative
import Data.Char

-- Parser Type
newType Parser a = P (String -> [(a, String)])

-- function to remove the dummy construcor "The P"
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- Parsing primative item
-- item is the basic bulding block of all other parsers
item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])
--  > parse item ""
-- []

-- > parse item "abc"
-- [(’a’,"bc")]

instance Functor Parser where
-- fmap :: (a -> b) -> Parser a -> Parser
  fmap g p = P (\inp -> case parse p inp of
                         [] -> []
                         [(v,out)] -> [(g v, out)])

-- > parse (fmap toUpper item) "abc"
-- [(’A’,"bc")]

-- > parse (fmap toUpper item) ""
-- []
 instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                     [] -> []
                     [(g,out)] -> parse (fmap g px) out)
-- > parse (pure 1) "abc"
-- [(1,"abc")]

three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x,z)
-- three :: Parser (Char,Char)
-- three = do x <- item
--            item
--            z <- item
--            return (x,z)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                         [] -> []
                         [(v,out)] -> parse (f v) out)

-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f

instance Alternative Maybe where
   -- empty :: Maybe a
   empty = Nothing
   -- (<|>) :: Maybe a -> Maybe a -> Maybe a
   Nothing <|> my = my
   (Just x) <|> _ = Just x

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
  [] -> parse q inp
  [(v,out)] -> [(v,out)])

-- examples
-- > parse empty "abc"
-- []

-- > parse (item <|> return ’d’) "abc"
-- [(’a’,"bc")]

-- > parse (empty <|> return ’d’) "abc"
-- [(’d’,"abc")]

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

-- example
-- > parse (char ’a’) "abc"
-- [(’a’,"bc")]

 string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
-- example
--  > parse (string "abc") "abcdef"
-- [("abc","def")]

-- > parse (string "abc") "ab1234"
-- []

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()
-- examples
-- > parse ident "abc def"
-- [("abc"," def")]

-- > parse nat "123 abc"
-- [(123," abc")]

-- > parse space " abc"
-- [((),"abc")]

int :: Parser Int
int = do char ’-’
         n <- nat
         return (-n)
         <|> nat
-- example
-- > parse int "-123 abc"
-- [(-123," abc")]

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

-- example
-- > parse nats " [1, 2, 3] "
-- [([1,2,3],"")]
-- > parse nats "[1,2,]"
-- []

expr :: Parser Int
expr = do t <- term
          do symbol "+"
          e <- expr
          return (t + e)
          <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
          t <- term
          return (f * t)
          <|> return

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
             [(n,[])] -> n
             [(_,out)] -> error ("Unused input " ++ out)
             [] -> error "Invalid input"

-- examples
-- > eval "2*3+4"
-- 10
-- > eval "2*(3+4)"
-- 14
-- > eval "2*3^4"
-- ***Exception: Unused input
-- > eval "one plus two"
-- ***Exception: Invalid input

box :: [String]
box = ["+---------------+",
       "| |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

-- Buttons
buttons :: String
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

-- showbox
showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

-- display
display xs = do writeat (3,2) (replicate 13 ’ ’)
                writeat (3,2) (reverse (take 13 (reverse xs)))

-- calc
calc :: String -> IO ()
calc xs = do display xs
          c <- getCh
          if elem c buttons then
              process c xs
          else
              do beep
                 calc xs
-- process
process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC" = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n" = eval xs
             | elem c "cC" = clear
             | otherwise = press c xs

-- quit
quit :: IO ()
quit = goto (1,14)

-- delete
delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

-- eval
eval :: String -> IO ()
eval xs = case parse expr xs of
             [(n,[])] -> calc (show n)
             _ -> do beep
                     calc xs
-- clear
clear :: IO ()
clear = calc []

-- press
press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

-- run
run :: IO ()
run = do cls
         showbox
         clear
