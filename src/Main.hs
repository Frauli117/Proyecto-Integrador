module Main where

import Inventario.Modelo (ConOrigen(..), Entidad, Relacion, Regla, ErrorFormato(..), Seccion(..))
import Inventario.Parser (Linea, separarSecciones, parsearEntidad, parsearRelacion, parsearRegla)

main :: IO ()
main = do
  contenido <- readFile "problema.txt"
  let secciones = separarSecciones contenido
      lineasEntidades = concat [lineas | (Entidades, lineas) <- secciones]
      lineasRelaciones = concat [lineas | (Relaciones, lineas) <- secciones]
      lineasReglas = concat [lineas | (ReglasSolicitadas, lineas) <- secciones]
  mapM_ mostrarResumen secciones
  putStrLn "\nEntidades interpretadas:"
  mapM_ mostrarEntidad (map parsearEntidad lineasEntidades)
  putStrLn "\nRelaciones interpretadas:"
  mapM_ mostrarRelacion (map parsearRelacion lineasRelaciones)
  putStrLn "\nReglas interpretadas:"
  mapM_ mostrarRegla (map parsearRegla lineasReglas)

mostrarResumen :: (Seccion, [Linea]) -> IO ()
mostrarResumen (seccion, lineas) =
  putStrLn (nombreSeccion seccion ++ ": " ++ show (length lineas) ++ " lineas")

nombreSeccion :: Seccion -> String
nombreSeccion Entidades = "Entidades"
nombreSeccion Relaciones = "Relaciones"
nombreSeccion ReglasSolicitadas = "Reglas solicitadas"

mostrarEntidad :: Either ErrorFormato (ConOrigen Entidad) -> IO ()
mostrarEntidad resultado =
  case resultado of
    Right (ConOrigen numero _ entidad) ->
      putStrLn ("Linea " ++ show numero ++ ": " ++ show entidad)
    Left (ErrorFormato numero mensaje) ->
      putStrLn ("Linea " ++ show numero ++ ": ERROR - " ++ mensaje)

mostrarRelacion :: Either ErrorFormato (ConOrigen Relacion) -> IO ()
mostrarRelacion resultado =
  case resultado of
    Right (ConOrigen numero _ relacion) ->
      putStrLn ("Linea " ++ show numero ++ ": " ++ show relacion)
    Left (ErrorFormato numero mensaje) ->
      putStrLn ("Linea " ++ show numero ++ ": ERROR - " ++ mensaje)

mostrarRegla :: Either ErrorFormato (ConOrigen Regla) -> IO ()
mostrarRegla resultado =
  case resultado of
    Right (ConOrigen numero _ regla) ->
      putStrLn ("Linea " ++ show numero ++ ": " ++ show regla)
    Left (ErrorFormato numero mensaje) ->
      putStrLn ("Linea " ++ show numero ++ ": ERROR - " ++ mensaje)
