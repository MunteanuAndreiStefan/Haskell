{-
Exercitiul 4

Definiti functia tokenize, care primeste la intrare un sir de
caractere si intoarce o valoare de tip Maybe [Token]:

- Nothing daca sirul dat ca parametru nu contine tokenuri valide; 

- Just l daca l :: [Token] este chiar lista de tokenuri din sirul dat
la intrare.

Exemple:

-- > tokenize "1+2"
-- Just [TInt "1", TSuma, TInt "2"]
-- > tokenize "1+2*3"
-- Just [TInt "1", TSuma, TInt "2", TProdus, TInt "3"]
-- > tokenize "12+23*34"
-- Just [TInt "12", TSuma, TInt "23", TProdus, TInt "34"]
-- > tokenize "12 + 23 * 34"
-- Just [TInt "12", TSuma, TInt "23", TProdus, TInt "34"]
-- > tokenize " 12 + 23 * 34"
-- Just [TInt "12", TSuma, TInt "23", TProdus, TInt "34"]
-- > tokenize "     12 + 23 * 34"
-- Just [TInt "12", TSuma, TInt "23", TProdus, TInt "34"]
-- > tokenize "     12 +   23      *   34    "
-- Just [TInt "12", TSuma, TInt "23", TProdus, TInt "34"]
-- > tokenize "12+2 3*34"
-- Just [TInt "12", TSuma, TInt "2", TInt "3", TProdus, TInt "34"]
-- > tokenize "1 2 3 +"
-- Just [TInt "1", TInt "2", TInt "3", TSuma]
-- > tokenize "a1 2 3 +"
-- Nothing
-- > tokenize "1 _ 2 3 +"
-- Nothing
-- > tokenize "1 / 2"
-- Nothing

Incercati si alte exemple.

Hint: daca v este o valoare de tip Maybe a, puteti face o analiza de cazuri folosind
constructia "case" in felul urmator:

case v of
  Just x  -> 1
  Nothing -> 2

Expresia de mai sus se evalueaza la 1 daca v este de forma Just x si
la 2 daca v este Nothing.

-}
import Data.Char


data Token = TInt String
           | TSuma
           | TProdus
             deriving Show
             
             
skipWhiteSpace :: String -> String
skipWhiteSpace (' ':xs) = skipWhiteSpace xs
skipWhiteSpace s = s

getInteger :: String -> (String, String)
getInteger (x:xs)
   | isDigit x = let (xs', ys') = getInteger(xs) in (x:xs', ys')
getInteger s = ("", s)


tokenNext :: String -> Maybe (Token, String)
tokenNext (x:xs) 
    | isDigit x = let (xs', ys') = getInteger(x:xs) in Just (TInt xs', ys')
    | x=='+' = Just (TSuma, xs)
    | x=='*' = Just (TProdus, xs)
tokenNext _ = Nothing

mydiv :: Int -> Int -> Maybe Int
mydiv _ 0 = Nothing
mydiv a b = Just (a `div` b)

add_and_div :: Int -> Int -> Maybe Int
add_and_div a b = case (mydiv a b) of
    Nothing -> Nothing
    Just x -> Just(x+a)

tokenize :: String -> Maybe [Token]
tokenize a = if (skipWhiteSpace a) == ""
                then Just []
             else case (tokenNext(skipWhiteSpace(a))) of
                Just (t, rest) -> case (tokenize rest) of
                                        Nothing -> Nothing
                                        Just tokens -> Just (t:tokens)
                Nothing -> Nothing