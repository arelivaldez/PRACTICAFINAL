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
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion _ = undefined

-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones _ = undefined


--combina :: []
--combina [] b= []
--combina (x:xs) b = (b:x):(combina xs b)

--asignarvalor ::
--asignarvalor [] [] = []
--asignarvalor (x:xs) (y:ys) = (x,y) :(asignarvalor xs ys)

--asignarInterp ::
--asignarInterp xs [ ]= []
--asignarInterp 


--combinacionesAux :: Int  -> [[Bool]] -> [[Bool]]

--combinacionesAux 0 xs = xs
--combinacionesAux n xs = combinacionesAux (n-1) ((combina x False) ++ (combina xs True)


-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad _ = undefined


--tablaDeVerdadAux (x:xs) (y:ys) = (x,y)
-----------------------------------------------------


