data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables  (Atom var) = [var]
variables (Neg f)= conjunto (variables f) 
variables (f1 :&: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :|: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :=>: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :<=>: f2) = conjunto (variables f1 ++ variables f2)

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs , y /= x ] 

-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom var) = Neg (Atom var)
negacion (Neg f) = f  
negacion (f1 :&: f2) = negacion f1 :|: negacion f2  
negacion (f1 :|: f2) = negacion f1 :&: negacion f2  
negacion (f1 :=>: f2) = f1 :&: negacion f2  
negacion (f1 :<=>: f2) = (f1 :&: negacion f2) :|: (negacion f1 :&: f2)  

-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom var) = (Atom var)
equivalencia (Neg f) =  negacion ( equivalencia f)
equivalencia (f1 :&: f2) = equivalencia f1 :&: equivalencia f2  
equivalencia (f1 :|: f2) =  equivalencia f1  :|: equivalencia f2  
equivalencia (f1 :=>: f2) = negacion (equivalencia f1)  :|: equivalencia f2  
equivalencia (f1 :<=>: f2) = (negacion (equivalencia f1) :|: equivalencia f2) :&: ( equivalencia f1 :|: negacion (equivalencia f2))  
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
buscarVar :: Var -> [(Var, Bool)] -> Bool
buscarVar var [] = error "No todas las variables estan definidas"
buscarVar var ((v, val):xs) = if var == v
                                then val
                                else buscarVar var xs

interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom var) asignaciones = buscarVar var asignaciones
interpretacion (Neg f) asignaciones = not (interpretacion f asignaciones)
interpretacion (f1 :&: f2) asignaciones = interpretacion f1 asignaciones && interpretacion f2 asignaciones
interpretacion (f1 :|: f2) asignaciones = interpretacion f1 asignaciones || interpretacion f2 asignaciones
interpretacion (f1 :=>: f2) asignaciones = not (interpretacion f1 asignaciones) || interpretacion f2 asignaciones
interpretacion (f1 :<=>: f2) asignaciones = interpretacion f1 asignaciones == interpretacion f2 asignaciones


-------------------- EJERCICIO 5 --------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


combina :: [[Bool]] -> Bool -> [[Bool]]
combina [] a = [] 
combina (x:xs) a = (a : x) : combina xs a

asignarvalor :: [Var] -> [Bool] -> [(Var, Bool)]
asignarvalor [] [] = []  
asignarvalor (x:xs) (y:ys) = (x, y) : asignarvalor xs ys  

asignarInterp :: [Var] -> [[Bool]] -> [[(Var, Bool)]]
asignarInterp xs [] = []  
asignarInterp xs (y:ys) = ((asignarvalor xs y) : (asignarInterp xs ys) )

combinacionesAux :: Int -> [[Bool]] -> [[Bool]]
combinacionesAux 0 (x:xs) = (x:xs)  
combinacionesAux n (x:xs) = combina (x:xs) True ++ combina (x:xs) False

combinaciones :: Formula -> [[(Var, Bool)]]
combinaciones xs = asignarInterp (variables xs) (combinacionesAux (longitud (variables xs)) [[False], [True]])

-------------------- EJERCICIO 6 --------------------
--combinacionesInterp :: Formula -> [[(Var,Bool)]] -> [Bool]
--combinacionesInterp fs [] = []
--combinacionesInterp fs (x:xs) = [(interpretacion fs x)] ++ (combinacionesINterp fs xs)

--tablaDeVerdadAux :: [[(Var,Bool)]] -> [Bool] -> [([(Var,Bool)],Bool)]
--tablaDeVerdadAux xs [] = []
--tablaDeVerdadAux (x:xs) (y:ys) = (x,y) tabladeVerdadAux xs ys

--tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
--tablaDeVerdad _ = undefined


-----------------------------------------------------




