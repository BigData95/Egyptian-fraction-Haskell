 
module Main where
import Control.Monad
import System.Directory
import System.IO
import Data.Char
main::IO()

main = undefined


{-
Practica 2 Haskell

Autor: Martinez Vargas Edgar Ivan

Al inicio hice que el programa leia un entero del teclado para que sea el denominador, luego otro para que sea el denominador
Pero al final cambia para que se pueda ingresar una cadena tipo a/b y el "programa" hace las trasnformaciones a los tipos necesarios

El "programa" inicia llamando a menu. Tiene varias opciones y se puede agregar nuevas fracciones y nuevas fracciones egipcias.
Se puede hacer esto gracias a que se escribre en un archivo, uno para las fracciones egipcias otro para la fraccion.

El loop principal quiza se pueda meter a una funcion y hacer "menu" mas limpio, pero soy sincero: me dio pereza

Cada vez que se agrega una nueva funcion y se quiere transforma a egipcia , se elimina el archivo que tenia la funcion egipcia anterior
porque si lo sobreescribia hacia un bug raro que no se como solucionar de otra manera.  

-}
menu = do
       putStr "------------------------------------------------------\n\
                \Bienvenido al programa de Fracciones egipcias!!!! \n\
                \---------------------------------------------------\n\
                \Por favor ingresa una fraccion de la siguiente manera: 3/4 \n" 
       frac <- getLine
       writeFile "fraccion.txt" frac
       putStrLn ("Tu fraccion es : " ++ frac )
       
       let loop = do
                    opcion <- pideOpcion
                    putStr "\n-----------------------------------------\n"
                    putStrLn ("Elegiste " ++ (show opcion))   
                    putStr "-----------------------------------------\n"                 
                    existEgip <- doesFileExist "egipcia.txt"
                    existFrac <- doesFileExist "fraccion.txt" --Nunca se usa porque siempre existe
                    case opcion of

                         "1" -> do
                                putStrLn "Dame una fraccion de la siguiente manera: 3/4"
                                f <- getLine 
                                writeFile "fraccion.txt" f
                                
                         "2" -> do
                                 lista <- readFile "fraccion.txt"
                                 putStr lista                
                         "3" -> do
                                   if(existEgip)
                                       then removeFile "egipcia.txt"
                                       else return ()
                                   r <- readFile "fraccion.txt"
                                   let list = egip(aFraccion(r))
                                   egipcia <- openFile "egipcia.txt" ReadWriteMode 
                                   hPrint egipcia list
                                   hClose egipcia
                                   putStr "---------------------------------------------------\n\
                                           \-------------------Archivo creado-------------------\n\
                                           \Puedes ver el archivo si seleccion las opcion 4\n"        
                         "4" -> do
                                  if(existEgip) 
                                        then do
                                              lista <- readFile "egipcia.txt"
                                              putStr "---------------------------------------------------\n\
                                                      \La fraccion egipcia es la siguiente \n"
                                              putStr lista
                                        else putStr "\n----------AUN NO CREAS EL ARCHIVO---------------\n\n"       
                         "5" -> do
                                putStrLn "Ingresa la fraccion egipcia.\nPor ejemplo [1/2,1/4,1/5]"
                                f <- getLine
                                writeFile "egipcia.txt" (show (read f :: FraccEgip))
                         "6" -> do
                                  if(existEgip)
                                       then do
                                            r <- readFile "egipcia.txt"
                                            print(normal(aListFraccion(r)))
                                       else putStr "\n----------AUN NO CREAS EL ARCHIVO---------------\n\n"   
                         "7" -> do --AL final siempre se elimina todos los archivos
                                    removeFile "fraccion.txt"  --Siempre va existir                        
                                    if(existEgip)
                                        then removeFile "egipcia.txt"
                                        else return ()
                                    putStr "\n-----------------Adios-----------------\n"
                         _ -> putStr "\n---------POR FAVOR ESCOJE UNA OPCION VALIDA--------------------\n\n"
                    
                    when ( opcion /= "7") loop
       loop       
       

            
pideOpcion = 
    do
     putStr "\n---------------------------------------------------\n\
              \Ingresa una digito para selecionar una opcion\n\
              \---------------------------------------------------\n\
              \Menu de opciones: \n\
              \1-Ingresa una nueva fraccion\n\
              \2-Lee la fraccion desde archivo \n\
              \3-Convertir a fraccion Egipcia y escribir en archivo\n\
              \4-Leer una fraccion egipcia desde archivo\n\
              \5-Introduce una fraccion egipcia desde el teclado\n\
              \6-Transformal fraccion egipcia a normal\n\
              \7-Salir del programa\n"
     opcion <- getLine
     return opcion


data Fraccion = Fraccion Int Int deriving (Eq)

instance Show Fraccion where show (Fraccion a b) = (show a) ++ "/" ++ (show b)
type FraccEgip = [Fraccion]
type FracPrueba = Fraccion


instance Read Fraccion where
            readsPrec _ = readFraccion

readFraccion :: String ->[(Fraccion,String)]          
readFraccion str = [(Fraccion (read n)(read d),resto)]
              where 
                n = tomaNum str
                (d,resto) = tomaDen str

tomaNum str = takeWhile isDigit str
tomaDen str = (takeWhile isDigit resto, dropWhile isDigit resto)
             where
                resto = tail(dropWhile (/='/') str)        

            
     
simplificar (Fraccion a b) = Fraccion (a `quot` factor) (b `quot` factor)
                       where factor = gcd a b
            
----suma--------------------------------------------------------------
suma :: Fraccion -> Fraccion -> Fraccion
suma (Fraccion a b) (Fraccion x y) = if (b == y)
                                       then Fraccion(a+x)(b)
                                       else Fraccion(a*y + b*x)(b*y)

----resta-------------------------------------------------------------
resta :: Fraccion -> Fraccion -> Fraccion
resta (Fraccion a b) (Fraccion x y) = if(b == y)
                                       then Fraccion(a-x)(b)
                                       else Fraccion(a*y - b*x)(b*y)
---De fraccion a egipcia                                            
egip :: Fraccion -> [Fraccion]                       
egip (Fraccion a b)
                  | (a == 1) = [Fraccion 1 b]
                  | otherwise = x : egip(y) 
                        where x = Fraccion (1) ( (b `div` a) + 1 )   --conv(Fraccion a b)
                              y = simplificar(resta (Fraccion a b)(x))
 
----De egipcia a Fraccion 
normal :: [Fraccion] -> Fraccion                      
normal [Fraccion a b] = Fraccion a b
normal (x:xs) = simplificar((suma x (normal xs)))



------------------------------Todo esto es para leer del archivo y tranformarlo 
------------------------------ya sea de string a fraccion o de string a lista de fracciones(Egipcia)



-----------PARA STRING A [FRACCION]-------------------------------------------------------
--Condicion para crear la lista
esComaDigit x = x==',' || isDigit x

--Quita las / de la lista de : "[1/3,1/4]" pasa a "[13,14]" , pero sigue siendo cadena
removeNon :: String -> String
removeNon xs = filter (esComaDigit) xs

--Toma una cadena de numeros y los transforma a entero 
scanString :: String -> Int
scanString = go 0
    where go a [] = a
          go a (x:xs) | 0 <= sc && sc <= 9 = go (10*a+sc) xs
                      | otherwise = 0
              where sc = scanChar x
scanChar :: Char -> Int      --Auxiliar para scanString
scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

--Toma una lista de numeros y lo transforma a lista de enteros : "[12,14,120]" -> [12,14,120] 
toEntero [] = []
toEntero (xs) = scanString(takeWhile (/=',') (xs)):toEntero(tail1(dropWhile (/=',') (xs) ))
tail1 [] = []
tail1 (x:xs) = xs


--Pasa de lista de enteros a lista de Fraccion
cambio [] = []
cambio (x:xs) = Fraccion (a) (b) : cambio(xs)
                where a = head (digs x)
                      b = number(tail(digs x))

                      
--Pasa de String a una lista de Fraccion   "[1/2,1/20,1/23]" -> [1/2,1/20,1/23] tipo Fraccion 
aListFraccion :: String -> [Fraccion]
aListFraccion xs = cambio(toEntero(removeNon(xs)))  

--Pasa de entero a digitos
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

--Pasa de digitos a entero
number xs = foldl1 (\x y -> 10*x+y) xs 

-----------------------Para STRING A [Fraccion]--------------------------

--Pasa de String a lista
removeSlash [] = []
removeSlash xs =   takeWhile (/='/') xs : removeSlash(tail1(dropWhile (/='/') xs )  )
                      

--de lista de string a lista de enteros
--listEntero :: [String] -> [Integer]
alistEntero [] = []
alistEntero (x:xs) = scanString(x): alistEntero xs

--De [int,int] a  Fraccion a b
change xs = Fraccion (xs!!0) (xs!!1)

--Para de string a Fraccion  "3/4" a 3/4 tipoFraccion
aFraccion xs = change(alistEntero(removeSlash(xs)))              




