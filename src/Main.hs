{-#LANGUAGE ScopedTypeVariables#-}
 
module Main where
import Control.Monad
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Directory
import System.IO
main::IO()

main = do
       putStr "------------------------------------------------------\n\
                \Bienvenido al programa de Fracciones egipcias!!!! \n\
                \---------------------------------------------------\n\
                \Por favor ingresa una fraccion\n\
                \Primero el numerador: "
       --hFlush stdout
       a :: Int <- readLn
       putStr "Luego el denominador: "
       b :: Int <- readLn 
       putStr "------------------------------------------------------\n\
                \Tu fraccion es : "  
       print (Fraccion a b)
       let loop = do 
                    putStr "---------------------------------------------------\n\
                            \Ingresa una digito para selecionar una opcion\n\
                            \---------------------------------------------------\n\
                            \Menu de opciones: \n\
                            \1-Convertir a fraccion Egipcia y escribir en archivo\n\
                            \2-Leer una fraccion egipcia desde archivo\n\
                            \3-Transformal fraccion egipcia a normal\n\
                            \4-Salir del programa\n"
                    prueba <- getLine
                    bool <- doesFileExist "egipcia.txt"
                    let list = egip(Fraccion a b)
                    case prueba of
                         "1" -> do 
                                   egipcia <- openFile "egipcia.txt" WriteMode
                                   hPrint egipcia list
                                   hClose egipcia
                                   putStr "---------------------------------------------------\n\
                                             \---------------------------------------------------\n\
                                            \-------------------Archivo creado-------------------\n\n"
                         "2" -> do
                                  --bool <- doesFileExist "egipcia.txt"
                                  if(bool) 
                                        then do
                                              lista <- readFile "egipcia.txt"
                                              putStr "---------------------------------------------------\n\
                                                      \La fraccion egipcia es la siguiente \n"
                                              putStr lista
                                        else putStr "\n----------AUN NO CREAS EL ARCHIVO---------------\n\n"       
                         "3" -> do
                                  --bool <- doesFileExist "egipcia.txt"
                                  if(bool) 
                                        then print(normal(list))
                                        else putStr "\n----------AUN NO CREAS EL ARCHIVO---------------\n\n"   
                         "4" -> do 
                                if(bool)
                                    then removeFile "egipcia.txt"
                                    else return ()
                                putStr "\n-----------------Adios-----------------\n"
                         _ -> putStr "\n---------POR FAVOR ESCOJE UNA OPCION VALIDA--------------------\n\n"
                          --otherwise -> exDefault
                    when ( prueba /= "4") loop
       loop       
       
       
       

------------------DEFINA UN TIO DE DATOS QUE GUARDE UNA FRACCION---------------------------------
----------2. Haskell Avanzado
--------------1. Defina un tipo de datos que guarde una fracción
--------------2. Defina las operaciones +;-;x; = para las fracciones que definió.

data Fraccion = Fraccion Int Int deriving (Eq)
instance Show Fraccion where show (Fraccion a b) = (show a) ++ "/" ++ (show b)

data Fraccion2 = Fraccion2 Int Int deriving (Eq)
instance Show Fraccion2 where show (Fraccion2 a b) = "Fraccion " ++ (show a) ++ " " ++ (show b)

            
     
simplificar (Fraccion a b) = Fraccion (a `quot` factor) (b `quot` factor)
                       where factor = gcd a b
            

----multiplicacion-----------------------------------------------------
multi :: Fraccion -> Fraccion -> Fraccion
multi (Fraccion a b)  (Fraccion x y) = Fraccion(a*x) (b*y)

-----division----------------------------------------------------------
divi :: Fraccion -> Fraccion -> Fraccion
divi (Fraccion a b ) (Fraccion x y) = Fraccion(a*y) (b*x)

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
                                       



conv :: Fraccion -> Fraccion
conv (Fraccion a b) = Fraccion (1) (x+1)
            where x = ( b `div` a)                       
          
egip :: Fraccion -> [Fraccion]                       
egip (Fraccion a b)
                  | (a == 1) = [Fraccion 1 b]
                  | otherwise = x : egip(y) 
                        where x = Fraccion (1) ( (b `div` a) + 1 )   --conv(Fraccion a b)
                              y = simplificar(resta (Fraccion a b)(x))
 
normal :: [Fraccion] -> Fraccion                      
normal [Fraccion a b] = Fraccion a b
normal (x:xs) = simplificar((suma x (normal xs)))





