longitud :: [t]->Int
longitud t = length t

ultimo :: [t]->t
ultimo t | length t == 1 = head t
         | otherwise = ultimo (tail t)



pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys 

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales (x:xs) | xs == [] = True
                    | x == head xs = todosIguales xs
                    | otherwise = False

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos (x:xs) | pertenece x xs == True = False
                      | otherwise = todosDistintos xs  

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs == True = True
                    | otherwise = hayRepetidos xs   

quitar:: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys) | x == y = ys
                | otherwise = y : (quitar x ys) 

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y:ys) | x == y =  quitarTodos x ys
                     | otherwise = y : (quitarTodos x ys)

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs == True = eliminarRepetidos xs
                         | otherwise = x : eliminarRepetidos xs

mismosElementos :: (Eq t)=>[t] -> [t] -> Bool
mismosElementos [][] = True
mismosElementos (x:xs) (y:ys) |