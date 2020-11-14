----------------HASKELL-CUADRADOS-MÁGICOS-DEYORS-AGOSTO-2020--------------
------------------------------CUADRADOS MÁGICOS---------------------------
import System.Random
import Data.List -- Para sort y transpose
type Tabla = [[Int]]

tablaInicialJugador1 :: Tabla
tablaInicialJugador1 = [[0,-1,-1],[-1,-1,-1],[-1,-1,-1]]

tablaInicialJugador2 :: Tabla
tablaInicialJugador2 = [[-1,-1,-1],[-1,-1,-1],[-1,-1,-1]]

tablaInicialOrdenador :: Tabla
tablaInicialOrdenador = [[-1,-1,-1],[-1,-1,-1],[-1,-1,-1]]


-----FUNCIONES PARA SACAR LAS SUMAS DE FILAS, COLUMNAS Y DIAGONALES-------

--FUNCIONES AUXILIARES

{- Dada una tabla devuelve una lista con la suma de sus filas.-}
sumaFilas :: Tabla -> [Int]
sumaFilas tab = map sum tab

{- Dada una tabla devuelve una lista con la suma de sus columnas.-}
sumaColumnas :: Tabla -> [Int]
sumaColumnas tab = sumaFilas (transpose tab)

{- Dada una tabla devuelve su diagonal principal.-}
diagonal1 :: Tabla -> [Int]
diagonal1 ((x1:_):xs) = x1 : diagonal1 [tail x | x <- xs]
diagonal1 _ = []

{- Dada una tabla devuelve su diagonal secundaria.-}
diagonal2 :: Tabla -> [Int]
diagonal2 tab = reverse (diagonal1 (reverse tab))

{- Dada una diagonal, devuelve la suma de sus elementos.-}
sumaDiagonal :: [Int] -> Int
sumaDiagonal xs = sum xs

{- Dada una tabla, saca todas las sumas de sus filas, columnas y 
diagonales, las pone en una lista y las ordena.-}

preMaximoIgual :: Tabla -> [Int]
preMaximoIgual tab = sort (l1 ++ l2 ++ l3 ++ l4)
     where l1 = sumaFilas tab
           l2 = sumaColumnas tab
           l3 = [sumaDiagonal (diagonal1 tab)]
           l4 = [sumaDiagonal (diagonal2 tab)]

{- Dada una lista ordenada, saca otra lista con la cantidad de veces que 
se repite un numero.-}

maximoIgual :: [Int] -> Int -> [Int] -> [Int]
maximoIgual [x,y] max_mom lista_max
     |(x == y) = lista_max ++ [(max_mom + 1)]
     |otherwise = lista_max ++ [max_mom,1]
maximoIgual (x:xs) max_mom lista_max
     |(x == head xs) = maximoIgual xs (max_mom + 1) lista_max
     |otherwise = maximoIgual xs 1 (lista_max ++ [max_mom])

--FUNCION FINAL

{- Dada una tabla, devuelve el máximo número de columnas, filas y 
diagonales con sumas iguales.-}

sumaTabla :: Tabla -> Int
sumaTabla tab = maximum (maximoIgual (preMaximoIgual tab) 1 [])


-------------------INTELIGENCIA ARTIFICIAL DEL ORDENADOR------------------

{- Dado un elemento y una lista, intercala el primer elemento entre todas 
las posiciones posibles de la lista.-}

intercalador :: Int -> [Int] -> [[Int]]
intercalador x [] = [[x]]
intercalador x (y:ys) = (x:y:ys) : [y:zs | zs <- intercalador x ys]

{- Dada una lista, devuelve una lista de listas de todas las permutaciones
de la primera lista.-}

permutaLista :: [Int] -> [[Int]]
permutaLista [] = [[]]
permutaLista (x:xs) = concat [intercalador x ys | ys <- permutaLista xs]

{- Dada una lista de listas y el número por el que quieras empezar, te 
añade todas las posibilidades de números del 1 al 9 que se pueden añadir
al final de cada lista.-}

ultimaOpcion :: [[Int]] -> Int -> [[Int]]
ultimaOpcion [] _ = []
ultimaOpcion (x:xs) 10 = (ultimaOpcion xs 1)
ultimaOpcion (x:xs) e = (x ++ [e]):(ultimaOpcion (x:xs) (e+1))

{- Dado el tablero del jugador, saca una lista de listas con todas las 
opciones que tiene el ordenador para jugar.-}

sacaJugadasOrdenador :: Tabla -> [[Int]]
sacaJugadasOrdenador tab =
     let opciones = sacaOpciones tab
         posibilidades = permutaLista opciones
         jugadasFinales = ultimaOpcion posibilidades 1
     in jugadasFinales

{- Dada una tabla y una opcion de resolucion posible para el ordenador, 
calcula el resultado que puede ocasionar esa opcion y devuelve una 
lista con la opcion y el valor final de filas y columnas maximas que 
puede ocasionar en formato [jugada1, jugada2,..., jugada5, valor_final].-}

pruebaJugada :: Tabla -> [Int] -> [Int]
pruebaJugada tab (x1:x2:x3:x4:x5:_) =
     let tab1 = cambiaValor tab (0,1) x1
         tab2 = cambiaValor tab1 (1,2) x2
         tab3 = cambiaValor tab2 (2,1) x3
         tab4 = cambiaValor tab3 (1,0) x4
         tabFinal = cambiaValor tab4 (1,1) x5
         maxFlCoDi = sumaTabla tabFinal
     in x1:x2:x3:x4:x5:maxFlCoDi:[]

{- Dada una tabla y todas las opciones para el ordenador, calcula todos
los resultados de tableros que pueden ocasionar esas opciones y devuelve
una lista con la opcion y el valor final de filas y columnas maximas que
puede ocasionar en formato:
[ [jugada1, jugada2,..., jugada5, valor_final] , ... ].-}

pruebaJugadas :: Tabla -> [[Int]] -> [[Int]]
pruebaJugadas tab [] = []
pruebaJugadas tab (x:xs) =
     let jugada = pruebaJugada tab x
     in (jugada:(pruebaJugadas tab xs))

{- Ordenacion de las posibilidades de mayor a menor numero de filas, 
columas y diagonales iguales.-}

specialSort :: Ord a => [a] -> [a] -> Ordering
specialSort (x1:x2:x3:x4:x5:x6:_) (y1:y2:y3:y4:y5:y6:_)
     |x6 < y6 = GT
     |x6 == y6 = EQ
     |otherwise = LT

ordenaPosibilidades :: [[Int]] -> [[Int]]
ordenaPosibilidades pos = sortBy specialSort pos


--FUNCION FINAL

{-Dada una tabla del jugador y otra del ordenador, devuelve la mejor
jugada del ordenador.-}

mejorJugada :: Tabla -> Tabla -> [Int]
mejorJugada tabJ tabO = 
     let jugadasO = sacaJugadasOrdenador tabJ
         posibilidades = pruebaJugadas tabO jugadasO
         mejorJugadaO = head (ordenaPosibilidades posibilidades)
     in mejorJugadaO

---------------------FUNCIONES GENÉRICAS PARA LAS TABLAS------------------

{- Dada una tabla, coordenadas y valor a cambiar devuelve nueva tabla con
valor cambiado en las coordenadas especificadas.-}

cambiaValor :: Tabla -> (Int,Int) -> Int -> Tabla
cambiaValor tab (fila,col) val = izq ++ [medio] ++ der
     where izq = take fila tab
           medio = izq1 ++ [val] ++ der1
                 where izq1 = take col (tab!!fila)
                       der1 = drop (col + 1) (tab!!fila)
           der = drop (fila + 1) tab

{- Dado un caracter te comprueba si esta entre los valores que has 
especificado (En String).-}

comprobarNumero :: Char -> String -> IO Int
comprobarNumero numero comprobante
     |(numero == 'E') || (numero == 'e') = error "Has salido del juego. Hasta luego!"
     |not (numero `elem` comprobante) = do
         putStrLn "\nNo entiendo la instruccion que me has dado. Prueba otra vez!\n"
         numero <- getChar
         comprobarNumero numero comprobante
     |otherwise = do
         return (read (numero:[]) :: Int)

{- Dado un String del nombre de un jugador, comprueba que es válido 
(no sobrepasa 31 letras ni es vacío).-}

comprobarNombre :: String -> IO String
comprobarNombre nombre
     |(length nombre) > 31 = do
                         putStrLn "Este nombre es demasiado largo. Intenta que no supere 31 letras:\n"
                         nombre1 <- getLine
                         comprobarNombre nombre1
     |(length nombre) == 0 = do
                         putStrLn "El nombre no puede ser vacio!. Prueba otra vez:\n"
                         nombre1 <- getLine
                         comprobarNombre nombre1
     |otherwise = return (nombre)


{-Dada una lista, te da un número aleatorio de esa lista.-}
numeroAleatorio :: [Int] -> IO Int
numeroAleatorio xs = do
     i <- randomRIO (0, length xs-1)
     return (xs !! i)

{-Dada una tabla busca el numero que quieras empezando por las cordenadas 
que quieras y te devuelve las coordenadas donde se ha encontrado por 
primera vez.-}

buscaElemento :: Tabla -> Int -> (Int,Int) -> (Int,Int)
buscaElemento tab x (a,b)
     | (tab !! a !! b == x) = (a,b)
     | (b /= 2) = buscaElemento tab x (a,(b+1))
     | otherwise = buscaElemento tab x ((a+1),0)

-----------------------------------RONDA 1--------------------------------

--AUXILIARES


{- Dadas dos tablas y dos valores, según donde esté el cero de la primera 
tabla esta función reemplaza la posición de ese cero por el valor 
introducido, en ambas tablas, y devuelve las tablas cambiadas. También 
genera los ceros correspondientes a las sigueintes posiciones de la Ronda 1.-}

actualizaRonda1 :: (Tabla,Tabla) -> Int -> Int -> (Tabla,Tabla)
actualizaRonda1 (tab1, tab2) x1 x2
     |((buscaElemento tab1 0 (0,0)) == (0,0)) = 
         let tab1' = (cambiaValor tab1 (0,0) x1)
             tab1Final = (cambiaValor tab1' (0,2) 0)
             tab2Final = (cambiaValor tab2 (0,0) x2)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (0,2)) = 
         let tab1' = (cambiaValor tab1 (0,2) x1)
             tab1Final = (cambiaValor tab1' (2,2) 0)
             tab2Final = (cambiaValor tab2 (0,2) x2)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (2,2)) = 
         let tab1' = (cambiaValor tab1 (2,2) x1)
             tab1Final = (cambiaValor tab1' (2,0) 0)
             tab2Final = (cambiaValor tab2 (2,2) x2)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (2,0)) = 
         let tab1Final = (cambiaValor tab1 (2,0) x1)
             tab2Final = (cambiaValor tab2 (2,0) x2)
         in (tab1Final,tab2Final)
     |otherwise = (tab1,tab2)

{- Dadas dos tablas y un valor, según donde esté el cero de la primera tabla 
esta función reemplaza la posición de ese cero por el valor introducido, y en
la otra tabla pone una x donde esta la posicion del cero de la primera.-}

actualizaRonda1J1 :: (Tabla,Tabla) -> Int -> (Tabla,Tabla)
actualizaRonda1J1 (tab1, tab2) x
     |((buscaElemento tab1 0 (0,0)) == (0,0)) = 
         let tab1Final = (cambiaValor tab1 (0,0) x)
             tab2Final = (cambiaValor tab2 (0,0) 0)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (0,2)) = 
         let tab1Final = (cambiaValor tab1 (0,2) x)
             tab2Final = (cambiaValor tab2 (0,2) 0)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (2,2)) = 
         let tab1Final = (cambiaValor tab1 (2,2) x)
             tab2Final = (cambiaValor tab2 (2,2) 0)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (2,0)) = 
         let tab1Final = (cambiaValor tab1 (2,0) x)
             tab2Final = (cambiaValor tab2 (2,0) 0)
         in (tab1Final,tab2Final)
     |otherwise = (tab1,tab2)

{- Dadas dos tablas y un valor, según donde esté el cero de la segunda tabla 
esta función reemplaza la posición de ese cero por el valor introducido, y en
la primera tabla pone una x en la siguiente posicion del cero de la primera.-}

actualizaRonda1J2 :: (Tabla,Tabla) -> Int -> (Tabla,Tabla)
actualizaRonda1J2 (tab1, tab2) x
     |((buscaElemento tab2 0 (0,0)) == (0,0)) = 
         let tab1Final = (cambiaValor tab1 (0,2) 0)
             tab2Final = (cambiaValor tab2 (0,0) x)
         in (tab1Final,tab2Final)
     |((buscaElemento tab2 0 (0,0)) == (0,2)) = 
         let tab1Final = (cambiaValor tab1 (2,2) 0)
             tab2Final = (cambiaValor tab2 (0,2) x)
         in (tab1Final,tab2Final)
     |((buscaElemento tab2 0 (0,0)) == (2,2)) = 
         let tab1Final = (cambiaValor tab1 (2,0) 0)
             tab2Final = (cambiaValor tab2 (2,2) x)
         in (tab1Final,tab2Final)
     |((buscaElemento tab2 0 (0,0)) == (2,0)) = 
         let tab1Final = tab1
             tab2Final = (cambiaValor tab2 (2,0) x)
         in (tab1Final,tab2Final)
     |otherwise = (tab1,tab2)

-----FUNCIONES FINALES

ronda1Ordenador :: (Tabla,Tabla)-> Int -> IO (Tabla,Tabla)
ronda1Ordenador (tab1,tab2) 0 = do
     printJuegoJO tab1 tab2
     putStrLn "\n                        Se ha acabado la RONDA 1!"
     return (tab1,tab2)
ronda1Ordenador (tab1,tab2) x = do
     printJuegoJO tab1 tab2
     putStrLn "\nEscribe el numero del 1 al 9 que ira en la posicion marcada.\n"
     numero <- getChar
     numeroJugador <- (comprobarNumero numero "123456789")
     putStrLn "\n\nEl ordenador esta pensando..."
     numeroOrdenador <- (numeroAleatorio [1,2,3,4,5,6,7,8,9])
     putStrLn "El ordenador ha hecho su jugada!"
     ronda1Ordenador (actualizaRonda1 (tab1,tab2) numeroJugador numeroOrdenador) (x-1)


ronda1Jugadores :: (String,String) -> (Tabla,Tabla) -> Int -> IO (Tabla,Tabla)
ronda1Jugadores (n1,n2) (tab1,tab2) 0 = do
     putStrLn "\n\n"
     printJuegoJJ (n1,n2) tab1 tab2
     putStrLn "\n                        Se ha acabado la RONDA 1!"
     return (tab1,tab2)
ronda1Jugadores (n1,n2) (tab1,tab2) x = do
     printJuegoJJ (n1,n2) tab1 tab2
     putStrLn ("\n " ++ n1 ++ "! Escribe el numero del 1 al 9 que ira en la posicion marcada.\n")
     numero <- getChar
     numeroJ1 <- (comprobarNumero numero "123456789")
     let (tab1',tab2') = actualizaRonda1J1 (tab1,tab2) numeroJ1
     putStrLn "\n\n"
     printJuegoJJ (n1,n2) tab1' tab2'
     putStrLn ("\n " ++ n2 ++ "! Escribe el numero del 1 al 9 que ira en la posicion marcada.\n")
     numero2 <- getChar
     numeroJ2 <- (comprobarNumero numero2 "123456789")
     let (tab1'',tab2'') = actualizaRonda1J2 (tab1',tab2') numeroJ2
     ronda1Jugadores (n1,n2) (tab1'',tab2'') (x-1)

-----------------------------------RONDA 2--------------------------------


--FUNCIONES AUXILIARES

{- Dada una tabla, saca los numeros de las posiciones de las esquinas, lo 
que serán las opciones del otro jugador.-}

sacaOpciones :: Tabla -> [Int]
sacaOpciones tab = 
     let x1 = tab !! 0 !! 0
         x2 = tab !! 0 !! 2
         x3 = tab !! 2 !! 0
         x4 = tab !! 2 !! 2
     in sort [x1,x2,x3,x4]

{- Imprime coherentemente las opciones de la función anterior en pantalla.-}
muestraOpciones :: [Int] -> [Char]
muestraOpciones [x,y] = (show(x) ++ " y " ++ show(y) ++ ".")
muestraOpciones [x] = show(x) ++ "."
muestraOpciones (x:xs) = (show(x) ++ ", " ++ muestraOpciones(xs))

intAString :: [Int] -> String
intAString [] = ""
intAString (x:xs) = ((show x) ++ (intAString xs))

{- Dada una lista y un elemento, te devuelve otra lista sin la primera vez 
que se encuentre el elemento.-}

eliminaElemento :: [Int] -> Int -> [Int]
eliminaElemento [] e = []
eliminaElemento (x:xs) e
     |(x == e) = xs
     |otherwise = x:(eliminaElemento xs e)

{- Dadas dos tablas y dos valores, según donde esté el cero de la primera 
tabla esta función reemplaza la posición de ese cero por el valor introducido,
en ambas tablas, y devuelve las tablas cambiadas. También genera los ceros 
correspondientes a las sigueintes posiciones de la Ronda 2.-}

actualizaRonda2 :: (Tabla,Tabla) -> Int -> Int -> (Tabla,Tabla)
actualizaRonda2 (tab1, tab2) x1 x2
     |((buscaElemento tab1 0 (0,0)) == (0,1)) = 
         let tab1' = (cambiaValor tab1 (0,1) x1)
             tab1Final = (cambiaValor tab1' (1,2) 0)
             tab2Final = (cambiaValor tab2 (0,1) x2)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (1,2)) = 
         let tab1' = (cambiaValor tab1 (1,2) x1)
             tab1Final = (cambiaValor tab1' (2,1) 0)
             tab2Final = (cambiaValor tab2 (1,2) x2)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (2,1)) = 
         let tab1' = (cambiaValor tab1 (2,1) x1)
             tab1Final = (cambiaValor tab1' (1,0) 0)
             tab2Final = (cambiaValor tab2 (2,1) x2)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (1,0)) = 
         let tab1Final = (cambiaValor tab1 (1,0) x1)
             tab2Final = (cambiaValor tab2 (1,0) x2)
         in (tab1Final,tab2Final)
     |otherwise = (tab1,tab2)

actualizaRonda2J1 :: (Tabla,Tabla) -> Int -> (Tabla,Tabla)
actualizaRonda2J1 (tab1, tab2) x
     |((buscaElemento tab1 0 (0,0)) == (0,1)) = 
         let tab1Final = (cambiaValor tab1 (0,1) x)
             tab2Final = (cambiaValor tab2 (0,1) 0)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (1,2)) = 
         let tab1Final = (cambiaValor tab1 (1,2) x)
             tab2Final = (cambiaValor tab2 (1,2) 0)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (2,1)) = 
         let tab1Final = (cambiaValor tab1 (2,1) x)
             tab2Final = (cambiaValor tab2 (2,1) 0)
         in (tab1Final,tab2Final)
     |((buscaElemento tab1 0 (0,0)) == (1,0)) = 
         let tab1Final = (cambiaValor tab1 (1,0) x)
             tab2Final = (cambiaValor tab2 (1,0) 0)
         in (tab1Final,tab2Final)
     |otherwise = (tab1,tab2)

actualizaRonda2J2 :: (Tabla,Tabla) -> Int -> (Tabla,Tabla)
actualizaRonda2J2 (tab1,tab2) x
     |((buscaElemento tab2 0 (0,0)) == (0,1)) = 
         let tab1Final = (cambiaValor tab1 (1,2) 0)
             tab2Final = (cambiaValor tab2 (0,1) x)
         in (tab1Final,tab2Final)
     |((buscaElemento tab2 0 (0,0)) == (1,2)) = 
         let tab1Final = (cambiaValor tab1 (2,1) 0)
             tab2Final = (cambiaValor tab2 (1,2) x)
         in (tab1Final,tab2Final)
     |((buscaElemento tab2 0 (0,0)) == (2,1)) = 
         let tab1Final = (cambiaValor tab1 (1,0) 0)
             tab2Final = (cambiaValor tab2 (2,1) x)
         in (tab1Final,tab2Final)
     |((buscaElemento tab2 0 (0,0)) == (1,0)) = 
         let tab1Final = tab1
             tab2Final = (cambiaValor tab2 (1,0) x)
         in (tab1Final,tab2Final)
     |otherwise = (tab1,tab2)


--FUNCIONES FINALES

ronda2Ordenador :: (Tabla,Tabla)-> [Int] -> [Int] -> IO (Tabla,Tabla)
ronda2Ordenador (tab1,tab2) [] _ = do
     printJuegoJO tab1 tab2
     putStrLn "\n                        Se ha acabado la RONDA 2!"
     return (tab1,tab2)
ronda2Ordenador (tab1,tab2) opciones (mejorJugadaOrd:xs) = do
     printJuegoJO tab1 tab2
     putStrLn ("\nLos numeros que tienes disponibles son: " ++ (muestraOpciones opciones))
     putStrLn "\nEscribe el numero que quieras que vaya en la posicion marcada.\n"
     numero <- getChar
     numeroJugador <- (comprobarNumero numero (intAString opciones))
     ronda2Ordenador (actualizaRonda2 (tab1,tab2) numeroJugador mejorJugadaOrd) (eliminaElemento opciones numeroJugador) xs

ronda2Jugadores :: (String,String) -> (Tabla,Tabla) -> ([Int],[Int]) -> IO (Tabla,Tabla)
ronda2Jugadores (n1,n2) (tab1,tab2) ([],[]) = do
     printJuegoJJ (n1,n2) tab1 tab2
     putStrLn "\n                        Se ha acabado la RONDA 2!"
     return (tab1,tab2)
ronda2Jugadores (n1,n2) (tab1,tab2) (opcionesJ1,opcionesJ2) = do
     printJuegoJJ (n1,n2) tab1 tab2
     putStrLn (("\n " ++ n1 ++ "! Los numeros que tienes disponibles son: " ++ (muestraOpciones opcionesJ1)))
     putStrLn "\nEscribe el numero que quieras que vaya en la posicion marcada.\n"
     numero1 <- getChar
     numeroJ1 <- (comprobarNumero numero1 (intAString opcionesJ1))
     let (tab1',tab2') = actualizaRonda2J1 (tab1,tab2) numeroJ1
     printJuegoJJ (n1,n2) tab1' tab2'
     putStrLn (("\n " ++ n2 ++ "! Los numeros que tienes disponibles son: " ++ (muestraOpciones opcionesJ2)))
     putStrLn "\nEscribe el numero que quieras que vaya en la posicion marcada.\n"
     numero2 <- getChar
     numeroJ2 <- (comprobarNumero numero2 (intAString opcionesJ2))
     let (tab1'',tab2'') = actualizaRonda2J2 (tab1',tab2') numeroJ2
     ronda2Jugadores (n1,n2) (tab1'',tab2'') ((eliminaElemento opcionesJ1 numeroJ1), (eliminaElemento opcionesJ2 numeroJ2))

-----------------------------------RONDA 3--------------------------------

ronda3Ordenador :: (Tabla,Tabla) -> Int -> IO (Tabla,Tabla)
ronda3Ordenador (tab1,tab2) numeroOrdenador = do
     printJuegoJO tab1 tab2
     putStrLn "\nEscribe con ingenio el ultimo numero, del 1 al 9, que quieras usar para completar tu cuadrado.\n"
     numero <- getChar
     numeroJugador <- (comprobarNumero numero "123456789")
     let tabFinalJugador = cambiaValor tab1 (1,1) numeroJugador
         tabFinalOrdenador = cambiaValor tab2 (1,1) numeroOrdenador
     return (tabFinalJugador, tabFinalOrdenador)

ronda3Jugadores :: (String,String) -> (Tabla,Tabla) -> IO (Tabla,Tabla)
ronda3Jugadores (n1,n2) (tab1,tab2) = do
     printJuegoJJ (n1,n2) tab1 tab2
     putStrLn ("\n " ++ n1 ++ "! Escribe con ingenio el ultimo numero, del 1 al 9, que quieras usar para completar tu cuadrado.\n")
     numero1 <- getChar
     numeroJ1 <- (comprobarNumero numero1 "123456789")
     let tabFinalJ1 = cambiaValor tab1 (1,1) numeroJ1
         tabFinalJ2' = cambiaValor tab2 (1,1) 0
     printJuegoJJ (n1,n2) tabFinalJ1 tabFinalJ2'
     putStrLn ("\n " ++ n2 ++ "! Escribe con ingenio el ultimo numero, del 1 al 9, que quieras usar para completar tu cuadrado.\n")
     numero2 <- getChar
     numeroJ2 <- (comprobarNumero numero2 "123456789")
     let tabFinalJ2 = cambiaValor tab2 (1,1) numeroJ2
     return (tabFinalJ1, tabFinalJ2)

-------------------------MOSTRAR TABLERO EN PANTALLA----------------------

--(DURANTE EL JUEGO)
{-Dada la fila de una tabla, la representa durante el juego sus márgenes.-}
printFilaMedioPorTabla :: [Int] -> String
printFilaMedioPorTabla [x] 
     | (x == 0) = "|    x    |"
     | (x == -1) = "|         |"
     | otherwise = "|    " ++ (show x) ++ "    |"
printFilaMedioPorTabla (x:xs)
     | (x == 0) = "|    x    " ++ (printFilaMedioPorTabla xs)
     | (x == -1) = "|         " ++ (printFilaMedioPorTabla xs)
     | otherwise = "|    " ++ (show x) ++ "    " ++ (printFilaMedioPorTabla xs)

{-Dadas las filas de dos tablas, representa las filas completas que se 
verán durante el juego.-}

printFilaMedio :: [Int] -> [Int] -> String
printFilaMedio xs ys = (printFilaMedioPorTabla xs) ++ "          " ++ (printFilaMedioPorTabla ys)

{-Dado un numero, devuelve un String con ese número de espacios-}

espacios :: Int -> String
espacios 0 = ""
espacios n = " "++(espacios (n-1))

{-Dado un nombre válido, añade espacios a los lados de forma que quede 
centrado con printJuego.-}

centraNombre :: String -> String
centraNombre nombre
     |odd (length nombre) = esp1 ++ nombre ++ esp1
     |otherwise = esp2 ++ nombre ++ esp3
     where esp1 = espacios ((31-(length nombre)) `div` 2)
           esp2 = espacios ((30-(length nombre)) `div` 2)
           esp3 = espacios ((32-(length nombre)) `div` 2)

{-Dadas dos tablas, las representa enteramente como se verá durante el 
juego. Distingue entre los modos "JJ" (jugador contra jugador) o "JO" 
(jugador contra ordenador.-}

printJuegoJO :: Tabla -> Tabla -> IO()
printJuegoJO (x1:y1:z1:_) (x2:y2:z2:_) = do
     putStrLn "\n            JUGADOR               vs.               ORDENADOR"
     putStrLn "+---------+---------+---------+          +---------+---------+---------+"
     putStrLn (printFilaMedio x1 x2) 
     putStrLn "+---------+---------+---------+          +---------+---------+---------+"
     putStrLn (printFilaMedio y1 y2) 
     putStrLn "+---------+---------+---------+          +---------+---------+---------+"
     putStrLn (printFilaMedio z1 z2)
     putStrLn "+---------+---------+---------+          +---------+---------+---------+"

printJuegoJJ :: (String,String) -> Tabla -> Tabla -> IO()
printJuegoJJ (n1,n2) (x1:y1:z1:_) (x2:y2:z2:_) = do
     putStrLn ((centraNombre n1) ++ "    vs.   " ++ (centraNombre n2))
     putStrLn "+---------+---------+---------+          +---------+---------+---------+"
     putStrLn (printFilaMedio x1 x2) 
     putStrLn "+---------+---------+---------+          +---------+---------+---------+"
     putStrLn (printFilaMedio y1 y2) 
     putStrLn "+---------+---------+---------+          +---------+---------+---------+"
     putStrLn (printFilaMedio z1 z2)
     putStrLn "+---------+---------+---------+          +---------+---------+---------+"

--FUNCIONES AUXILIARES (AL FINAL DEL JUEGO)

{- Saca la penultima fila de la representación final, (la de las sumas de 
las columas y la diagonal principal).-}
printFilaColumnas :: [Int] -> String
printFilaColumnas [x] 
     |(length (show x) == 1) = "|   " ++ show(x) ++ "   |"
     |otherwise = "|  " ++ show(x) ++ "   |"
printFilaColumnas (x:xs)
     |(length (show x) == 1) = "|   " ++ show(x) ++ "   | " ++ (printFilaColumnas xs)
     |otherwise = "|  " ++ show(x) ++ "   | " ++ (printFilaColumnas xs)


--FUNCIONES FINALES (AL FINAL DEL JUEGO)

{- Representa enteramente una Tabla con las sumas de sus filas, columnas 
y diagonales -}
representarTablaFinal :: Tabla -> IO ()
representarTablaFinal tab = do
     let d1 = sumaDiagonal (diagonal1 tab)
         printd1_1d = "         " ++ (printFilaColumnas (sumaColumnas tab)) ++ "       |  " ++ (show d1) ++ "   |"
         printd1_2d = "         " ++ (printFilaColumnas (sumaColumnas tab)) ++ "       |  " ++ (show d1) ++ "  |"
         d2 = sumaDiagonal (diagonal2 tab)
         printd2_1d = "                                             |  " ++ (show d2) ++ "   |"
         printd2_2d = "                                             |  " ++ (show d2) ++ "  |"
         f1 = (sumaFilas tab) !! 0
         printf1_1d = "        " ++ (printFilaMedioPorTabla (tab !! 0)) ++ "  ->  |  " ++ (show f1) ++ "   |"
         printf1_2d = "        " ++ (printFilaMedioPorTabla (tab !! 0)) ++ "  ->  |  " ++ (show f1) ++ "  |"
         f2 = (sumaFilas tab) !! 1
         printf2_1d = "        " ++ (printFilaMedioPorTabla (tab !! 1)) ++ "  ->  |  " ++ (show f2) ++ "   |"
         printf2_2d = "        " ++ (printFilaMedioPorTabla (tab !! 1)) ++ "  ->  |  " ++ (show f2) ++ "  |"
         f3 = (sumaFilas tab) !! 2
         printf3_1d = "        " ++ (printFilaMedioPorTabla (tab !! 2)) ++ "  ->  |  " ++ (show f3) ++ "   |"
         printf3_2d = "        " ++ (printFilaMedioPorTabla (tab !! 2)) ++ "  ->  |  " ++ (show f3) ++ "  |"
     putStrLn "\n                                             + ---- +"
     if length (show d2) == 1 then do (putStrLn printd2_1d) else do (putStrLn printd2_2d)
     putStrLn "                                         _   + ---- +"
     putStrLn "                                          |"
     putStrLn "                                        /"
     putStrLn "        +---------+---------+---------+      + ---- +"
     if length (show f1) == 1 then do (putStrLn printf1_1d) else do (putStrLn printf1_2d)
     putStrLn "        +---------+---------+---------+      + ---- +"
     if length (show f2) == 1 then do (putStrLn printf2_1d) else do (putStrLn printf2_2d)
     putStrLn "        +---------+---------+---------+      + ---- +"
     if length (show f3) == 1 then do (putStrLn printf3_1d) else do (putStrLn printf3_2d)
     putStrLn "        +---------+---------+---------+      + ---- +"
     putStrLn "             |         |         |      \\"
     putStrLn "             v         v         v       _|"
     putStrLn "         + ----- + + ----- + + ----- +       + ---- +"
     if length (show d1) == 1 then do (putStrLn printd1_1d) else do (putStrLn printd1_2d)
     putStrLn "         + ----- + + ----- + + ----- +       + ---- +"

{- Representa la pantalla final de la modalidad contra el Ordenador.-}
representacionFinalOrdenador :: Tabla -> Tabla -> IO (Int,Int)
representacionFinalOrdenador tab1 tab2 = do
     putStrLn "\n                       TU CUADRADO ES:"
     representarTablaFinal tab1
     let maximoJ = sumaTabla tab1
         maximoOrd = sumaTabla tab2
     putStrLn ("\nEl maximo numero de filas, columas y diagonales iguales es " ++ (show (maximoJ)) ++ ".")
     putStrLn "\n               EL CUADRADO DE TU OPONENTE ES:"
     representarTablaFinal tab2
     putStrLn ("\nEl maximo numero de filas, columas y diagonales iguales es " ++ (show (maximoOrd)) ++ ".")
     return (maximoJ,maximoOrd)

representacionFinalJugadores :: (String,String) -> Tabla -> Tabla -> IO (Int,Int)
representacionFinalJugadores (n1,n2) tab1 tab2 = do
     putStrLn ("\n\nEl CUADRADO de " ++ n1 ++ " ES:")
     representarTablaFinal tab1
     let maximoJ1 = sumaTabla tab1
         maximoJ2 = sumaTabla tab2
     putStrLn ("\nEl maximo numero de filas, columas y diagonales iguales es " ++ (show (maximoJ1)) ++ ".")
     putStrLn ("\n\nEl CUADRADO de " ++ n2 ++ " ES:")
     representarTablaFinal tab2
     putStrLn ("\nEl maximo numero de filas, columas y diagonales iguales es " ++ (show (maximoJ2)) ++ ".")
     return (maximoJ1,maximoJ2)

------------------------------------JUEGO---------------------------------

resultado :: Int -> Int -> IO ()
resultado jugador ordenador
     |(jugador > ordenador) = do putStrLn "\n\nHAS GANADO!!!"
     |(jugador == ordenador) = do putStrLn "\n\nHas quedado empate!"
     |otherwise = do putStrLn "\n\nLo siendo, has perdido..."

resultadoJugadores :: (String,String) -> Int -> Int -> IO ()
resultadoJugadores (n1,n2) j1 j2
     |(j1 > j2) = do putStrLn ("\n\n" ++ n1 ++ " HA GANADO!!")
     |(j1 == j2) = do putStrLn "\n\nHABEIS QUEDADO EMPATE!!"
     |otherwise = do putStrLn ("\n\n" ++ n2 ++ " HA GANADO!!")

reintentar :: IO ()
reintentar = do 
     putStrLn "\n\nNo entiendo la instruccion que me has dado. Prueba otra vez!"
     letra <- getChar
     filtro letra

instrucciones :: IO()
instrucciones = do
     putStrLn "\n\nUn cuadrado magico es una matriz de 3x3 en la cual si sumamos sus filas, columnas y diagonales, se obtiene el mismo resultado."
     putStrLn "Puedes elegir si competir contra el ordenador o contra otro jugador."
     putStrLn "En una primera ronda, por turnos, tienes que escribir un numero a tu elección en cada una de las esquinas del cuadrado."
     putStrLn "En una segunda ronda, tendras que completar las casillas restantes del cuadrado con los numeros que haya escogido tu oponente, exceptuando la casilla central del cuadrado."
     putStrLn "Por ultimo, escribiras el numero que tu quieras en la casilla central del cuadrado."
     putStrLn "Si al sumar las filas, columnas y diagonales de tu cuadrado obtienes mas resultados iguales que tu contrincante, ¡habras ganado!"
     putStrLn "Pulsa M para volver al juego!"
     letra <- getChar 
     filtro letra

juegoOrdenador :: IO ()
juegoOrdenador = do
     putStrLn "\n\n------------------------------------------------------------------------"
     putStrLn "\n                                 RONDA 1!"
     (tabJugRonda1',tabOrdRonda1) <- ronda1Ordenador (tablaInicialJugador1,tablaInicialOrdenador) 4
-- Le ponemos ' porque el tablero del jugador no está listo para entrar en la Ronda 2 todavía, le falta la x.
     putStrLn "\n\n------------------------------------------------------------------------"
     putStrLn "\n                                 RONDA 2!"
     let tabJugRonda1 = cambiaValor tabJugRonda1' (0,1) 0 --Le hemos puesto el 0 (la x) en la coordenada correspondiente
         mejorJugadaOrd = mejorJugada tabJugRonda1 tabOrdRonda1
     (tabJugRonda2',tabOrdRonda2) <- ronda2Ordenador (tabJugRonda1,tabOrdRonda1) (sacaOpciones tabOrdRonda1) (mejorJugadaOrd)
     putStrLn "\n\n------------------------------------------------------------------------"
     putStrLn "\n                               ULTIMA RONDA!"
     let tabJugRonda2 = cambiaValor tabJugRonda2' (1,1) 0 --Le hemos puesto el 0 (la x) en la coordenada correspondiente
         numeroOrdenador = mejorJugadaOrd !! 4
     (tabJugFinal,tabOrdFinal) <- ronda3Ordenador (tabJugRonda2,tabOrdRonda2) numeroOrdenador
     (maximoJ,maximoOrd) <- representacionFinalOrdenador tabJugFinal tabOrdFinal
     resultado maximoJ maximoOrd
     putStrLn "\nPulsa P si quieres volver a jugar!"
     putStrLn "Pulsa E para salir.\n"
     letra <- getChar
     filtro letra

juegoJugadores :: IO()
juegoJugadores = do
     putStrLn "\n\nJugador 1! Introduce tu nombre:\n"
     nombre1 <- getLine
     nombreJ1 <- comprobarNombre nombre1
     putStrLn "\n\nJugador 2! Introduce tu nombre:\n"
     nombre2 <- getLine
     nombreJ2 <- comprobarNombre nombre2
     putStrLn "\n\n------------------------------------------------------------------------"
     putStrLn "\n                                 RONDA 1!"
     (tabJ1Ronda1',tabJ2Ronda1) <- ronda1Jugadores (nombreJ1,nombreJ2) (tablaInicialJugador1,tablaInicialJugador2) 4
-- Le ponemos ' porque el tablero del jugador 1 no está listo para entrar en la Ronda 2 todavía, le falta la x.
     putStrLn "\n\n------------------------------------------------------------------------"
     putStrLn "\n                                 RONDA 2!"
     let tabJ1Ronda1 = cambiaValor tabJ1Ronda1' (0,1) 0 --Le hemos puesto el 0 (la x) en la coordenada correspondiente
         opcionesJ2 = sacaOpciones tabJ1Ronda1
         opcionesJ1 = sacaOpciones tabJ2Ronda1
     (tabJ1Ronda2',tabJ2Ronda2) <- ronda2Jugadores (nombreJ1,nombreJ2) (tabJ1Ronda1,tabJ2Ronda1) (opcionesJ1,opcionesJ2)
     putStrLn "\n\n------------------------------------------------------------------------"
     putStrLn "\n                               ULTIMA RONDA!"
     let tabJ1Ronda2 = cambiaValor tabJ1Ronda2' (1,1) 0 --Le hemos puesto el 0 (la x) en la coordenada correspondiente
     (tabJ1Final,tabJ2Final) <- ronda3Jugadores (nombreJ1,nombreJ2) (tabJ1Ronda2,tabJ2Ronda2)
     (maximoJ1,maximoJ2) <- representacionFinalJugadores (nombreJ1,nombreJ2) tabJ1Final tabJ2Final
     resultadoJugadores (nombreJ1,nombreJ2) maximoJ1 maximoJ2
     putStrLn "\nPulsa J si quieres volver a jugar!"
     putStrLn "Pulsa E para salir.\n"
     letra <- getChar
     filtro letra

filtro :: Char -> IO ()
filtro letra 
     | (letra == 'P') || (letra == 'p') = do juegoOrdenador
     | (letra == 'J') || (letra == 'j') = do juegoJugadores
     | (letra == 'I') || (letra == 'i') = do instrucciones
     | (letra == 'M') || (letra == 'm') = do main
     | (letra == 'E') || (letra == 'e') = return ()
     | otherwise = do reintentar

main :: IO ()
main = do
     putStrLn "\nBienvenido a Los Cuadrados Magicos."
     putStrLn "Programado por DEYORS."
     putStrLn "Agosto del 2020."
     putStrLn "\nSi quieres jugar contra el ordenador, pulsa P"
     putStrLn "Si quieres jugar contra otro jugador, pulsa J"
     putStrLn "Si quieres saber como se juega, pulsa I"
     putStrLn "En cualquier momento puedes salir de la partida pulsando E\n"
     letra <- getChar
     filtro letra
