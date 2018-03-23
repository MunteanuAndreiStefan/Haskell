{-
In Haskell ne putem defini propriile tipuri de date. 
Acest lucru se poate realiza utilizand cuvantul cheie `data`. 

Un exemplu simplu este tipul de date predefinit `Bool`:

  data Bool = False | True 

* `Bool` este *numele* tipului de date; 
* `False` si `True` se numesc constructori;
* `|`, care se poate citi "sau", desparte constructorii posibili 
  ai tipului de date.

Atentie, tipul `Bool` este deja predefinit in Haskell. 
Daca introducem definitia de mai sus in ghci vom crea ambiguitati!
-}


{-
Urmatoarea definitie modeleaza tipul de date Lista de numere intregi:
-}

data Lista = ListaVida 
			| Cons Int Lista deriving (Show) -- show ca sa poata fi printat

{-
Observam ca tipul de date `Lista` are doi constructori: 
* `ListaVida`  - o constanta care corespunde listei fara nici un element;
* `Cons` - care construieste o noua lista dintr-un numar intreg
  si o alta lista.

EXERCITIU: Testati in ghci care sunt tipurile constructorilor. 
EXERCITIU: Cum arata elementele de tipul Lista?
Nota:  Daca in ghci vom scrie `ListaVida`, dupa apasarea tastei `Enter`
       vom primi o eroare. Motivul este ca, pentru noul tip de date definit,
       Haskell nu stie sa-l afiseze. Pentru moment putem scapa de aceasta
       eroare decomentand `deriving (Show)` de la sfarsitul definitiei tipului.
       Vom discuta pe larg despre ceea ce face cuvantul cheie `deriving` in
       laboratoarele urmatoare. 
-}

{- 
Pana in acest moment am vazut cum putem defini tipuri noi de date. 
Dar cum scriem functii peste aceste tipuri?
Urmatoarea functie calculeaza lungimea unei liste:
-}

lungime :: Lista -> Int
lungime ListaVida = 0  -- lista vida
lungime (Cons x xs) = 1 + lungime xs  -- lista compusa

{- 
Tipul functiei `lungime` arata ca functia primeste un argument de 
tip `Lista` (tip pe care tocmai l-am definit!) si returneaza un `Int`. 
Functia este definita pentru fiecare constructor al tipului `Lista`:
* daca argumentul este `ListaVida` atunci functia returneaza 0
* daca argumentul este de forma `Cons x xs`, unde `x` si `xs` sunt
  variabile de tip Int si respectiv Lista, atunci lungimea listei este
  calculata recursiv.

Vom detalia urmatorul apel de functie: 
  `lungime (Cons 10 (Cons 43 ListaVida))`
La apelarea functiei se va activa definitia pentru lista compusa:
   `lungime (Cons x xs) = 1 + lungime xs`
Aici, sablonul (sau 'pattern'-ul)
       (Cons x xs) 
se va potrivi peste 
       (Cons 10 (Cons 43 ListaVida))
astfel
  x  <- 10
  xs <- (Cons 43 ListaVida)

Asadar, `lungime (Cons 10 (Cons 43 ListaVida))` = 1 +  lungime (Cons 43 ListaVida).
La pasul urmator, aceeasi ramura va fi activata pentru functia lungime, dar 
acum x <- 43 iar xs <- ListaVida. La final, apelul `lungime ListaVida` va returna 0. 
Executia completa a functiei este:
`lungime (Cons 10 (Cons 43 ListaVida))` = 1 +  lungime (Cons 43 ListaVida) = 
                                        = 1 + 1 + lungime ListaVida = 2 + 0 = 2.`
EXERCITIU: Testati functia de mai sus.
-}

numara_aparitii :: Lista -> Int -> Int
numara_aparitii ListaVida _ = 0
numara_aparitii (Cons x xs)n = (numara_aparitii xs n) + if(x == n ) then 1 else 0


suma :: Lista -> Int
suma ListaVida = 0
suma (Cons x xs) = (suma xs) + x


maxim :: Lista -> Int
maxim ListaVida = 0
-- maxim (Cons x xs) = if((maxim xs) > x) then maxim xs else x
maxim (Cons x xs) = let max = maxim xs in if(max > x) then max else x


sortat_crescator :: Lista -> Bool
sortat_crescator ListaVida = True
sortat_crescator (Cons x ListaVida) = True
sortat_crescator (Cons x (Cons y xs)) = if(x>y) then False else sortat_crescator(xs)


concatenare :: Lista -> Lista -> Lista
concatenare ListaVida ListaVida = ListaVida -- Creste eficienta
concatenare xs ListaVida = xs -- Creste eficienta
concatenare (Cons x xs) xy = concatenare xs (Cons x xy)

data ArboreBinar = ArboreVid
                 | Nod a (ArboreBinar a) (ArboreBinar a) 
                 deriving (Show)
				 
inaltimeArb :: ArboreBinar a -> Integer
inaltimeArb ArboreVid = 0
inaltimeArb (Nod h _ _ _) = h

numar_noduri :: ArboreBinar a -> Int
numar_noduri (ArboreVid n)    = 1
numar_noduri (Nod x z) = numar_noduri x + numar_noduri z + 1

