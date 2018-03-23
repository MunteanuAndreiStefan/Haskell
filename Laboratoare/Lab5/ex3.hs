import Data.Char


data Token = TInt String
           | TSuma
           | TProdus
             deriving Show
             
{-

Exercitiul 3

Definiti functia tokenNext, care primeste la intrare un sir de caractere si
intoarce o valoare de tip Maybe (Token, String):

- valoarea intoarsa este Nothing daca sirul primul ca argument nu
incepe cu unul din tokenurile valide
- valoare intoarsa este Just (x, y) daca sirul primul ca argument incepe
cu tokenul x :: Token si restul sirului este y

Exemple:

-- > tokenNext "+ * 123"
-- Just (TSuma, " * 123")
-- > tokenNext "123123 12 +"
-- Just (TInt "123123", " 12 +")
-- > tokenNext "* 1 + 2"
-- Just (TProdus, " 1 + 2")
-- > tokenNext "a + 1"
-- Nothing
-- > tokenNext " 1 + 2"
-- Nothing
-}


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