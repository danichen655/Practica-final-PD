--HongXiang Chen

--1)
type Palabra = [Char]

data Hash = H [[(Palabra, Palabra)]]

--2)
instance Show Hash where show (H xx) = mostrarHash xx [0..9]

mostrarHash [] []= ""
mostrarHash [] (y:ys)= "|" ++ show y ++"| --->" ++ "\n" ++ mostrarHash [] (ys)
mostrarHash (x:xs) (y:ys) = "|" ++ show y ++"| --->" ++ mostrarTrans x ++ "\n" ++ mostrarHash xs (ys)

mostrarTrans :: Show a => [a] -> [Char]
mostrarTrans [] = ""
mostrarTrans (x:xs) = show x ++ " " ++ mostrarTrans xs

--3) Definicion de la funcion hash. Dada una palabra en espanol devuelve su valor en el indice de la
--      tabla. El indice tiene solo 10 valores distintos.

-- En esta funcion eligimos en que indice debe ir una palabra con su traduccion
-- La idea es sacar la posicion de la letra de cada palabra, por ejemplo 
-- la letra "a" es el indice 0 y la letra "z" es el indice 5, ya que esta en la posicion 25 y 
-- hacemos el modulo de 10
indice :: [Char] -> Int
indice palabra = (busquedaBinaria (['a'..'z']) (head palabra) 0 25) `mod` 10

-- Una busqueda binaria para encontrar la letra
busquedaBinaria :: Ord t => [t] -> t -> Int -> Int -> Int
busquedaBinaria xs letra ini fin
 |letra == xs!!medio = medio
 |letra > xs!!medio = busquedaBinaria xs letra (medio+1) fin
 |otherwise = busquedaBinaria xs letra ini (medio - 1)
 where 
 medio = (div(ini+fin) 2)

-- Funcion para convertir una lista a hash o diceversa.
listAHash :: [[(Palabra, Palabra)]] -> Hash
listAHash (x) = (H x)

hashToLists :: (Hash) -> [[([Char], [Char])]]
hashToLists (H x) = x


--4)Inicializaci´on del diccionario. Rellena la tabla hash donde se almacenara el diccionario a partir del
--  fichero de texto dado datos.txt.

-- una funcion auxilizar para poder poner las palabra con su indice
diccAux = ([[],[],[],[],[],[],[],[],[],[]])

inicializar :: IO Hash
inicializar = do
 content <- readFile "datos.txt" -- Nos devuelve en solo string todos los datos
 let linesOfFiles = words content -- separar por cada palabra
 -- convertir la lista en una lista de tuplas
 let aux = [(linesOfFiles!!x,linesOfFiles!!(x+1)) | x<-[0,2..(length linesOfFiles)-1]]
 return (listAHash(insert (aux)))

--Insertar cada tupla de palabra en el diccionario
insert xs = foldr (\x y -> insertAt  (x:[]) (indice(fst x)) (y)) diccAux xs

-- x es la tupla de palabras
-- i el indice a insertar
-- dicc el diccionario
-- Funcion que inserta en la posicion correspondiente, la idea es hacerle un hueco
-- con take y drop y luego hacemos concat para poder anhadir la tupla y lo que tenia antes
insertAt:: [a] -> Int -> [[a]] -> [[a]]
insertAt x i dicc = (take i dicc ++ concat [x, dicc!! i] : drop (i+1) dicc)

myDicc = (inicializar)

--5 Lectura de varias palabras a traducir introducidas por el teclado

lectura :: IO [String]
lectura = do
 putStr("Introduzca las palabras a traducir: ")
 content <- getLine
 let list = words content
 return (list)


--6) Calculo de la longitud media de las palabras introducidas.
mediaPalabras :: IO ()
mediaPalabras = do
 aux <- lectura
 let palabras = (length aux)
 let longitud = foldr(\x y -> length(x) + y) 0 aux
 let ret = (fromIntegral longitud) / (fromIntegral palabras)
 putStrLn("La longitud media es : " ++ show ret )

--7)Busqueda en el diccionario de la traduccion de las palabras introducidas.
busqueda = do
 a <- myDicc
 let dicc = hashToLists a
 palabras <- lectura
 let x = buscarPalabra (palabras) (dicc)
 return x

--buscarPalabra :: Palabra -> [[(Palabra, Palabra)]] -> [Palabra]
-- Llama a buscarPalabraAux con lista que debe estar y a cada palabra que hay que traduccir
buscarPalabra xs dicc = foldr (\x y -> (buscarPalabraAux (dicc!! (indice (x))) x) : y) [] xs

-- Funcion que buscar de palabra en palabra pasandole su indice correspondiente
buscarPalabraAux :: Eq t => [(t, String)] -> t -> String
buscarPalabraAux (x:xs) elem
 |fst x == elem = (snd x)
 |otherwise = if length xs == 0 then "Palabra no encontada" else buscarPalabraAux xs elem

--8) Mostrar las traducciones

traduccion :: IO ()
traduccion = do
 a <- busqueda
 putStrLn("Estas son las palabras traducidas: ")
 imprimir a 

imprimir :: (Foldable t, Show (t a)) => [t a] -> IO ()
imprimir (x:xs)= do
 if (length x) /= 0 then do
  print(x)
  if((length xs) /= 0) then do
   imprimir xs
   else do 
   putStrLn("Fin")
 else do
   putStrLn("Fin")