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
