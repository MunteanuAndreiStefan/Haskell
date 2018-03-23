{-

Am vazut cum se poate tokeniza un sir de caractere.

In continuare, vom trece la partea de parsare propriu-zisa: vom face o
functie parse care transforma un sir de tokenuri intr-o expresie
aritmetica.

Vom implementa un parser recursiv descendent foarte simplu.

-}

-- vom avea nevoie de modulul Data.Char petru diverse functii utile
import Data.Char

data Token = TInt String
           | TSuma
           | TProdus
             deriving Show

data Exp = Numar Int
         | Suma Exp Exp
         | Produs Exp Exp
         deriving Show

{-

Exercitiul 1

Pentru inceput, vom scrie un parser care va putea parsa produse de
factori, iar prin factor vom intelege pentru moment numere intregi.

Definiti functia parseFactor care primeste o lista de tokenuri si
intoarce o valoare de tip Maybe (Exp, [Token]) in felul urmator:

- Nothing daca lista de tokenuri primita ca argument nu incepe cu un
numar intreg

- Just (e, tl) daca lista de tokenuri primita ca argument incepe cu un
token de forma TInt s, e :: Exp este expresie formata doar din numarul
s iar tl sunt celelalte tokenuri (fara primul).

Exemple:

-- > parseFactor [TInt "123", TSuma, TInt "23"]
-- Just (Numar 123, [TSuma, TInt "23"])
-- > parseFactor [TSuma, TInt "23"]
-- Nothing
-- > parseFactor [TInt "23"]
-- Just (Numar 23, [])

Hint: pentru a converti un String intr-un Int, puteti folosi functia
predefinita read.

-- > read "123" :: Int
-- 123
-- > read "123" :: Float
-- 123.0
-- > :t read
-- > :i String

-}
parseFactor :: [Token] -> Maybe (Exp, [Token])
parseFactor(TInt s:rest) = Just(Numar (read s), rest)
parseFactor _ = Nothing
