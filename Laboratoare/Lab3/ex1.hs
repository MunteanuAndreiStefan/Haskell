{-

Mai jos avem un tip de date pentru expresii aritmetice cu numere
(intregi pe 32 de biti), + si *.

De exemplu, expresia matematica (3 + 4) * 5 este reprezentata de
valoarea Haskell:

-- > Produs (Suma (Numar 3) (Numar 4)) (Numar 5)
-- Produs (Suma (Numar 3) (Numar 4)) (Numar 5)

-}

data Exp = Numar Int
         | Suma Exp Exp
         | Produs Exp Exp
		 | Minus Exp Exp
		 | Impartit Exp Exp
         deriving Show

{-

Exercitiul 1

Cum reprezentam expresia aritmetica 4 * (3 + 2) * 2?

-}

--eval (Produs (Produs (Suma (Numar 3) (Numar 2)) (Numar 4)) (Numar 2))

{-

Dandu-se o expresie aritmetica, putem calcula valoarea acesteia
folosind functia "eval":

-}

eval :: Exp -> Maybe Int
eval (Numar n) = Just n
eval (Suma e1 e2) = case ((eval e1), (eval e2)) of
                    (Just n, Just m) -> Just (n + m)
                    (_,_) -> Nothing
					
eval (Produs e1 e2) = case ((eval e1), (eval e2)) of
                    (Just n, Just m) -> Just (n * m)
                    (_,_) -> Nothing
                
eval (Minus e1 e2) = case ((eval e1), (eval e2)) of
                    (Just n, Just m) -> Just (n - m)
                    (_,_) -> Nothing
					
eval (Impartit e1 e2) = case ((eval e1), (eval e2)) of
                    (_, Just 0) -> Nothing
                    (Just n, Just m) -> Just (quot n m)
                    (_,_) -> Nothing
{-

Exercitiul 2

Extindeti tipul Exp cu constructori pentru:

1) minus unar (e.g. MinusUnar (Numar 5) reprezinta expresia "-5")
2) minus binar (e.g. MinusBinar (Numar 3) (Numar 3) reprezinta expresia "-3")

Adaptati functia eval astfel incat sa trateze si cazul noilor
constructori.

Evaluati folosind functia voastra expresiile -(3 + 4) si ((-3) * 4) +
(10 * 2).

-}


{-

Exercitiul 3

Extindeti expresiile cu operatia de impartire.

Ce se intampla in cazul unei impartiri prin 0?

-}



{-
Tipuri parametrizate

Tipul parametrizat List:
> import Data.List
> :i []
data [] a = [] | a : [a]
...
In definitia tipul de date List, "a" este o variabila de tip. 
Prin urmare definitia listelor este parametrica in tipul elementelor.
...

Tipul parametrizat Maybe:

> :i Maybe
data Maybe a = Nothing | Just a
...
Tipul "Maybe a" contine toate valorile tipului "a", protejate de constructorul "Just",
precum si inca o valoare "Nothing". De exemplu, valorile de tip Maybe Int sunt
Nothing, Just 0, Just (-2), Just 7, ...

Cum ne-ar putea ajuta acest tip sa tratam cazul impartirii la zero?
-}
