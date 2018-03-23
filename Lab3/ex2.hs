import Data.Maybe

{-

Exercitiul 1

Scrieti o functie care primeste doua valori de tip Maybe Int si
intoarce o valoare de tip Maybe Int care contine catul impartirii,
daca aceasta se poate calcula, sau Nothing in caz contrar.

-}

impartireMaybe :: Maybe Int -> Maybe Int -> Maybe Int
impartireMaybe e1 e2 = case (e1, e2) of
                    (_, Just 0) -> Nothing
                    (Just n, Just m) -> Just (quot n m)
                    (_,_) -> Nothing
                    
                    --impartireMaybe (Just 5) (Just 10)
{-


Exercitiul 2

Aceeasi cerinta pentru functia sumaMaybe.

-}

sumaMaybe :: Maybe Int -> Maybe Int -> Maybe Int
sumaMaybe e1 e2 = case (e1, e2) of
            (Just n, Just m) -> Just ( n + m )
            (_,_) -> Nothing
{-

Mai jos avem un tip de date pentru expresii aritmetice cu numere
(intregi pe 32 de biti), "+" si "/".

-}

data Exp = Numar Int
         | Suma Exp Exp
         | Impartire Exp Exp
         deriving Show

{-

De exemplu, expresia matematica (3 + 4) / 5 este reprezentata de:
-- > Impartire (Suma (Numar 3) (Numar 4)) (Numar 5)
-- Impartire (Suma (Numar 3) (Numar 4)) (Numar 5)

Dandu-se o expresie aritmetica, putem calcula valoarea acesteia
folosind functia "eval":

-}


eval :: Exp -> Maybe Int
eval (Numar n) = Just n
eval (Suma e1 e2) = sumaMaybe (eval e1) (eval e2)
eval (Impartire e1 e2) = impartireMaybe (eval e1) (eval e2)

{-

De data aceasta, functia "eval" intoarce o valoare de tip "Maybe Int".
Daca expresia nu contine erori de tipul impartirii prin zero, atunci
rezultatul este "Just n", unde n este un Int care este valoarea
expresiei.  In caz contrar, daca expresia contine erori de tipul
impartirii prin zero, rezultatul este "Nothing".

-}

{-

Exercitiul 3

Incercati sa evaluati, folosind functia de mai sus, expresiile
aritmetice 10 / (3 + -3) si (4 + 5) / 3. Atentie! -3 este numarul
intreg -3 :: Int, nu expresia - 3, adica operatorul minus unar (pe
care nu-l avem deocamdata) aplicat numarului 3.


-}

{-

Exercitiul 4

Extindeti tipul Exp cu constructori pentru: minus unar, minus binar,
produs, modulo, radical (intreg), exponentiere, factorial si eventual
alte operatii interesante.  Adaptati functia eval astfel incat sa
trateze si cazul noilor constructori. Testati pe cateva exemple
relevante functiile de mai sus.

-}

radical :: Int -> Maybe Float
radical n
| n<0 = Nothing
| otherwise = Just (sqrt (fromIntegral n))
