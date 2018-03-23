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
import Data.Maybe
import System.Environment

-- valorile de tip Token corespund tuturor tokenurile de care avem nevoie
-- pentru inceput, vom trata doar '+', '*' si numerele intregi
data Token = TInt String
           | TSuma
           | TProdus
           | TMinusUnar
           | TParenLeft
           | TParenRight
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
skipWhiteSpace :: String -> String
skipWhiteSpace (' ':xs) = skipWhiteSpace xs
skipWhiteSpace s = s
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

tokenNext :: String -> Maybe (Token, String) 
tokenNext (x:xs) 
    |  isDigit x =  let (xs', ys') = getInteger(x:xs) in Just (TInt xs', ys')
    |  x == '+'  = Just (TSuma, xs)
    |  x == '*' = Just (TProdus, xs)
    |  x == '-' = Just (TMinusUnar, xs)
tokenNext _ = Nothing

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
tokenize :: String -> Maybe [Token] 
tokenize s = if (skipWhiteSpace s) == "" 
             then Just []
             else case tokenNext(skipWhiteSpace s) of
                  Just (t, rest) -> case (tokenize rest) of
                                         Just tokens -> Just (t:tokens)
                                         Nothing -> Nothing
                  Nothing -> Nothing


                  {-

Am vazut cum se poate tokeniza un sir de caractere.

In continuare, vom trece la partea de parsare propriu-zisa: vom face o
functie parse care transforma un sir de tokenuri intr-o expresie
aritmetica.

Vom implementa un parser recursiv descendent foarte simplu.

-}


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
{-parseFactor :: [Token] -> Maybe (Exp, [Token])
parseFactor (x:xs) = if (tokenize(x:xs)!!0) == Just (TInt x) then Just(Numar read x, tokenize(xs))
                     else Nothing-}

{-parseFactor :: [Token] -> Maybe (Exp, [Token])
parseFactor (TInt factor: rest) = Just (Numar (read factor), rest)
parseFactor _ = Nothing-}
{-

Exercitiul 2

Completati definitia functiei parseTerm de mai jos.

Scopul functiei este sa parseze un termen dintr-o lista de
tokenuri. Prin termen se intelege un produs de factor.

Functia intoarce Nothing daca lista de tokenuri nu incepe cu un termen
sau Just (e, tl) daca lista de tokenuri data ca argument incepe cu
termenul e si mai contine in plus tokenurile tl.

Exemple:

-- > parseTerm [TInt "123", TProdus, TInt "23"]
-- Just (Produs (Numar 123) (Numar 23), [])
-- > parseTerm [TInt "123", TProdus, TInt "23", TProdus, TInt "12"]
-- Just (Produs (Numar 123) (Produs (Numar 23) (Numar 12)), [])
-- > parseTerm [TInt "123"]
-- Just (Numar 123, [])
-- > parseTerm [TInt "123", TProdus]
-- Just (Numar 123, [TProdus])
-- > parseTerm [TProdus]
-- Nothing

Hint: variabilele sunt denumite in felul urmator:
1) te = tokens expression
2) el = expression left
3) trest = tokens rest
4) ter = tokens expression right
5) er = expression right

Mai aveti completat cazurile cand: 1) trest este lista vida si
respectiv 2) cand trest nu este lista vida, dar incepe cu altceva
decat TProdus.

-}


{-

Exercitiul 3.

Completati definitia functiei parseExp de mai jos.

Scopul functiei este sa parseze o expresie dintr-o lista de
tokenuri. Prin expresie se intelege o suma de termeni.

Exemple:

-- > parseExp [TInt "123", TProdus, TInt "1", TSuma, TInt "23"]
-- Just ((Suma (Produs (Numar 123) (Numar 1)) (Numar 23)), [])
-- > parseExp [TInt "123", TProdus, TInt "1"]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseExp [TInt "123", TProdus, TInt "1", TProdus, TInt "2", TSuma, TInt "23"]
-- Just ((Suma (Produs (Numar 123) (Produs (Numar 1) (Numar 2))) (Numar 23)), [])
-- > parseExp [TInt "123", TProdus, TInt "1", TProdus, TInt "2", TSuma, TInt "23", TProdus, TInt "2"]
-- Just ((Suma (Produs (Numar 123) (Produs (Numar 1) (Numar 2))) (Produs (Numar 23) (Numar 2))), [])
-- > parseExp [TInt "123", TProdus, TInt "1", TProdus, TInt "2", TSuma, TInt "23", TProdus, TInt "2", TSuma, TInt "1"]
-- Just ((Suma (Produs (Numar 123) (Produs (Numar 1) (Numar 2))) (Suma (Produs (Numar 23) (Numar 2)) (Numar 1))), [])
-- > parseExp [TInt "123", TSuma, TSuma]
-- Just ((Numar 123), [TSuma, TSuma])
-- > parseExp [TInt "123"]
-- Just ((Numar 123), [])
-- > parseExp [TInt "123", TProdus, TInt "1"]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseExp [TInt "123", TProdus]
-- Just ((Numar 123), [TProdus])

Hint: la fel cu parseTerm parseaza un produs de factori, la fel
parseExp trebuie sa parseze o suma de termeni, deci definitiile
functiilor parseExp si parseTerm seamana ca structura.

-}
parseTerm :: [Token] -> Maybe (Exp, [Token])
parseTerm te = case parseFactor te of
                 Just (el, trest) ->
                   case trest of
                     (TProdus:ter) -> case parseTerm ter of
                                       Nothing -> Just (el, TProdus:ter)
                                       Just (er, tl) -> Just ((Produs el er), tl)
                     (TSuma:ter) -> case parseTerm ter of
                                       Nothing -> Just (el, TSuma:ter)
                                       Just (er, tl) -> Just ((Suma el er), tl)
                     l -> Just(el, l)
                 Nothing -> Nothing

parseExp :: [Token] -> Maybe (Exp, [Token]) 
{-parseExp te = case parseTerm te of
                Just (el, trest) ->
                 case trest of 
                   (TProdus:ter) -> case parseExp ter of
                                      Nothing -> Just(el, TProdus:ter)
                                      Just (er, tl) -> Just ((Produs el er), tl)
                   (TSuma:ter) -> case parseTerm ter of
                                      Nothing -> Just(el, TSuma:ter)
                                      Just(er, tl) -> Just((Suma el er), tl)
                   l -> Just(el, l)
                Nothing -> Nothing-}

parseExp te = case (parseTerm te) of
                Just (t, rest) -> case rest of
                                   (TSuma:rest') -> case parseExp rest' of
                                                      Nothing -> Just(t, TSuma:rest')
                                                      Just (e, lt) -> Just (Suma t e, lt)
                                   l -> Just (t,l)
                Nothing -> Nothing
                 

{-}

Exercitiul 4

In acest moment, parserul nostru poate lucra doar pe doua nivele: sume
de produse de numere intregi. Totusi, o expresie poate fi mai
complicata decat atat. De exemplu: 1+2*(4+5). In mod necesar, o astfel
de expresie contine operatorul "+" sub operatorul "*" si deci are
nevoie de paranteze.

Extindeti tipul token cu un token pentru paranteze deschise si un
token pentru paranteze inchise.

-}


--parseFactor' :: [Token] -> Maybe (Exp, [Token])
parseFactor (TInt factor:rest) = Just (Numar (read factor), rest)
parseFactor(TParenLeft:rest) = case parseExp rest of
                                 Just(e, rest') -> case rest' of 
                                                     (TParenRight:rs) -> Just(e,rs)
                                                     l -> Just(e,l)
                                 Nothing -> Nothing
parseFactor _ = Nothing
{-

Exercitiul 5

In acest moment, prin definitie, un factor (in sensul functiei
parseFactor) este doar un numar intreg.

In continuare, prin factor vom intelege:
1) sau un numar intreg, ca pana acum;
2) sau o paranteza deschisa, urmata de o expresie, urmata de o paranteza inchisa.

Modificati functia parseFactor astfel incat sa parseze un factor in
sensul noii definitii de mai sus.

Exemple:

-- > parseFactor [TInt "123", TSuma, TInt "23"]
-- Just (Numar 123, [TSuma, TInt "23"])
-- > parseFactor [TSuma, TInt "23"]
-- Nothing
-- > parseFactor [TInt "23"]
-- Just (Numar 23, [])
-- > parseFactor [TParenLeft, TInt "23", TParenRight]
-- Just (Numar 23, [])
-- > parseFactor [TParenLeft, TParenLeft, TInt "23", TParenRight, TParenRight]
-- Just (Numar 23, [])
-- > parseFactor [TInt "123", TProdus, TInt "1", TSuma, TInt "23"]
-- Just ((Numar 123), [TProdus, TInt "1", TSuma, TInt "23"])

-- > parseFactor [TParenLeft, TInt "123", TProdus, TInt "1", TSuma, TInt "23", TParenRight]
-- Just ((Suma (Produs (Numar 123) (Numar 1)) (Numar 23)), [])
-- > parseFactor [TParenLeft, TInt "123", TProdus, TInt "1", TParenRight]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseFactor [TParenLeft, TParenLeft, TInt "123", TProdus, TInt "1", TParenRight, TParenRight]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseFactor [TInt "123", TSuma, TSuma]
-- Just ((Numar 123), [TSuma, TSuma])
-- > parseFactor [TInt "123"]
-- Just ((Numar 123), [])
-- > parseFactor [TInt "123", TProdus, TInt "1"]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseFactor [TInt "123", TProdus]
-- Just ((Numar 123), [TProdus])
-- > parseFactor [TParenLeft, TInt "123", TParenRight, TProdus]
-- Just ((Numar 123), [TProdus])
-- > parseFactor [TParenLeft, TParenLeft, TInt "123", TParenRight, TProdus]
-- Nothing
-- > parseFactor [TParenLeft, TParenRight]
-- Nothing
-- > parseFactor [TParenLeft]
-- Nothing
-- > parseFactor [TParenRight]
-- Nothing
-- > parseFactor [TSuma]
-- Nothing
-- > parseFactor [TProdus]
-- Nothing

-}

{-

Exercitiu 6

In acest moment, functia parseExp apeleaza recursiv parseTerm, care
apeleaza recursiv parseFactor, care apeleaza recursiv parseExp. Avem
deci 3 functii mutual recursive care pot parsa o expresie, oricat de
mare ar fi inaltimea arborelui sintactic al expresiei.

Folositi parseExp pentru a parsa lista de tokenuri din expresiile aritmetice:
1) 3 + 4 * (2 + 6);
2) (2 + 4) * (5 + 6) + 2 * 3;
3) 1 + (2 * 3 + 4) * (2 + 4 * 5);
4) 1 + 2 + 3 * (4 + 5 * (2 + 2 * (1 + 1))).

parseExp [TInt "3", TSuma, TInt "4", TProdus, TParenLeft, TInt "2", TSuma, TInt "6", TParenRight]
-}

{-

Exercitiul 7

Conectati tokenizatorul din fisierul anterior cu functiile de parsare
propriu-zise din acest fisier pentru a obtine functia parse :: String
-> Maybe Exp de mai jos.

Exemple:

-- > parse "2 +  3"
-- Just (Suma (Numar 2) (Numar 3))
-- > parse "(2 + )"
-- Nothing
-- > parse "_ + 2"
-- Nothing
-- > parse "2+3*4"
-- Just (Suma (Numar 2) (Produs (Numar 3) (Numar 4)))

Hint: atentie la tokenizarea parantezelor.

-}

parse :: String -> Maybe Exp 
parse exp = case ( tokenize exp) of 
                 Just tokenlist -> case (parseExp tokenlist) of
                                        Just (e, []) -> Just e
                                        _ -> Nothing
                 _ -> Nothing

{-

Exercitiul 8

Conectati functia eval (din al doilea fisier de azi) cu functia parse
pentru a obtine functia evalStr.

Exemple:

-- > evalStr "2+3"
-- Just 5
-- > evalStr "2*(3+4)"
-- Just 14
-- > evalStr "2++"
-- Nothing

-}



sumaMaybe :: Maybe Int -> Maybe Int -> Maybe Int
sumaMaybe e1 e2 = case (e1 , e2) of 
                       (Just e1, Just e2) -> Just (e1 + e2)

eval :: Exp -> Maybe Int
eval (Numar n) = Just n
eval (Suma e1 e2) = sumaMaybe (eval e1) (eval e2)


evalStr :: String -> Maybe Int
evalStr str = case parse str of
                Just e -> eval e
                _ -> Nothing

trim :: String -> String
trim = dropWhile ( == ' ') . reverse . dropWhile( == ' ') . reverse

main = do { 
  args <- getArgs;
  case args of 
    [arg] -> do  {
            content <- readFile arg;
            case (evalStr (trim content)) of 
              Just e -> putStrLn (show e)
              _ -> putStrLn "Invalid expression"
    }
    _ -> putStrLn "please provide a single file to execute"
}