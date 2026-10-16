module Main where

import Inventario.Modelo (Seccion(..))
import Inventario.Parser (Linea, separarSecciones)

main :: IO ()
main = do
  contenido <- readFile "problema.txt"
  let secciones = separarSecciones contenido
  mapM_ mostrarResumen secciones

mostrarResumen :: (Seccion, [Linea]) -> IO ()
mostrarResumen (seccion, lineas) =
  putStrLn (nombreSeccion seccion ++ ": " ++ show (length lineas) ++ " lineas")

nombreSeccion :: Seccion -> String
nombreSeccion Entidades = "Entidades"
nombreSeccion Relaciones = "Relaciones"
nombreSeccion ReglasSolicitadas = "Reglas solicitadas"
