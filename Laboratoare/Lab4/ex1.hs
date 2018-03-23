import qualified Data.Text as T

{-

Pana acum am vazut cum se poate crea un tip de date algebric pentru
expresii aritmetice si cum se pot evalua astfel de expresii.

Dar este incomod sa le scriem de mana.

Am avea nevoie de un parser, adica de un program care sa transforme
un sir de caractere reprezentand o expresie aritmetica intr-o valoare
Haskell de tip Exp.

De exemplu, rezultatul parsarii sirului de caractere "(3 + 4) * 5" ar
trebui sa fie "Produs (Suma (Numar 3) (Numar 4))".

Vom implementa parsarea in doua etape:

1) etapa de tokenizare
2) etapa de parsare propriu-zisa

In acest fisier vom implementa prima etapa.

Tokenizarea, sau analiza lexicala, transforma un sir de caractere
intr-un sir de tokenuri. De exemplu, "(33 + 424) * 5" va fi transformat
in:

1) paranteza deschisa
2) numarul 33
3) caracterul '+'
4) numarul 424
5) paranteza inchisa
6) caracterul '*'
7) numarul 5

-}

-- vom avea nevoie de modulul Data.Char petru diverse functii utile
import Data.Char

-- valorile de tip Token corespund tuturor tokenurile de care avem nevoie
-- pentru inceput, vom trata doar '+', '*' si numerele intregi
data Token = TInt String
           | TSuma
           | TProdus
             deriving Show

{-

Exercitiul 1

Definiti functia skipWhiteSpace, care primeste la intrare un sir de
caractere si intoarce sirul de caractere fara spatiile de la inceput.

Exemple:
-- > skipWhiteSpace "  3 +  5 "
-- "3 +  5 "
-- > skipWhiteSpace "2 + 4 "
-- "2 + 4 "

In rezolvare, tineti cont ca String nu este altceva decat un sinonim
pentru [Char] (un String este o lista de Char).

-}
{-
leftStrip = T.unpack . T.stripStart . T.pack

skipWhiteSpace :: String -> String
skipWhiteSpace text = leftStrip text
-}
skipWhiteSpace :: String -> String
skipWhiteSpace (' ':xs) = skipWhiteSpace xs
skipWhiteSpace s = s
