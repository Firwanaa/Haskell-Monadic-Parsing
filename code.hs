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
