import Data.Char

{-
Exercitiul 2

Definiti functia getInteger, care primeste la intrare un sir de caractere si
intoarce o pereche de siruri de caractere:

- prima componenta a perechii contine cifrele cu care incepe sirul
primit ca argument;
- a doua componenta, restul sirului.

Exemple:
-- > getInteger "123  + 23"
-- ("123", "  + 23")
-- > getInteger " 123 + 23 * 3"
-- ("", " 123 + 23 * 3")
-- > getInteger "123123123123 "
-- ("123123123123", " ")

Hint:
Ce face functia isDigit, care face parte din modulul Data.Char, pe care l-ati
importat la inceputul fisierului?
-- > isDigit '3'
-- > isDigit 'a'
-- > isDigit ' '
-}

getInteger :: String -> (String, String)
getInteger (x:xs)
   | isDigit x = let (xs', ys') = getInteger(xs) in (x:xs', ys')
getInteger s = ("", s)