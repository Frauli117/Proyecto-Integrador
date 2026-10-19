module Inventario.Parser (Linea, separarSecciones, parsearEntidad, parsearRelacion, parsearRegla) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)
import Inventario.Modelo (ConOrigen(..), Entidad(..), Relacion(..), Regla(..), ErrorFormato(..), Seccion(..))


type Linea = (Int, String)

separarSecciones :: String -> [(Seccion, [Linea])]
separarSecciones contenido =
  let numeradas = zip [1..] (lines contenido)
      clasificadas = clasificar Nothing numeradas
  in [ (seccion, [linea | (grupo, linea) <- clasificadas, grupo == seccion])
     | seccion <- [Entidades, Relaciones, ReglasSolicitadas]
     ]

clasificar :: Maybe Seccion -> [Linea] -> [(Seccion, Linea)]
clasificar _ [] = []
clasificar actual (linea@(_, texto):resto)
  | null limpio || take 1 limpio == "#" = clasificar actual resto
  | take 1 limpio == "[" = clasificar (reconocerSeccion limpio) resto
  | otherwise = case actual of
      Nothing -> clasificar actual resto
      Just seccion -> (seccion, linea) : clasificar actual resto
  where
    limpio = recortar texto

reconocerSeccion :: String -> Maybe Seccion
reconocerSeccion "[ENTIDADES]" = Just Entidades
reconocerSeccion "[RELACIONES]" = Just Relaciones
reconocerSeccion "[REGLAS_SOLICITADAS]" = Just ReglasSolicitadas
reconocerSeccion _ = Nothing

parsearEntidad :: Linea -> Either ErrorFormato (ConOrigen Entidad)
parsearEntidad (numero, texto) =
  case break (== ':') texto of
    (tipo, ':':nombreOriginal) ->
      let nombre = recortar nombreOriginal
      in if null nombre
         then errorEnLinea "Falta el nombre de la entidad"
         else if elem ':' nombre
              then errorEnLinea "Se esperaba un solo ':' entre el tipo y el nombre"
              else case recortar tipo of
                "producto" -> guardar (Producto nombre)
                "categoria" -> guardar (Categoria nombre)
                _ -> errorEnLinea ("Tipo de entidad desconocido: " ++ recortar tipo)
    _ -> errorEnLinea "Se esperaba el formato tipo: nombre"
  where
    guardar entidad = Right (ConOrigen numero texto entidad)
    errorEnLinea mensaje = Left (ErrorFormato numero mensaje)

parsearRelacion :: Linea -> Either ErrorFormato (ConOrigen Relacion)
parsearRelacion (numero, texto) =
  case break (== ':') texto of
    (tipo, ':':argumentos)
      | elem ':' argumentos ->
          errorEnLinea "Se esperaba un solo ':' entre el tipo y los valores"
      | otherwise ->
          case break (== ',') argumentos of
            (productoOriginal, ',':segundoOriginal) ->
              let producto = recortar productoOriginal
                  segundo = recortar segundoOriginal
              in if null producto || null segundo || elem ',' segundo
                 then errorEnLinea "Se esperaban dos valores no vacios separados por una sola coma"
                 else case recortar tipo of
                   "pertenece" -> guardar (Pertenece producto segundo)
                   "existencia" -> guardarCantidad Existencia producto segundo
                   "minimo" -> guardarCantidad Minimo producto segundo
                   _ -> errorEnLinea ("Tipo de relacion desconocido: " ++ recortar tipo)
            _ -> errorEnLinea "Se esperaba una coma entre los dos valores"
    _ -> errorEnLinea "Se esperaba el formato tipo: producto, valor"
  where
    guardar relacion = Right (ConOrigen numero texto relacion)
    errorEnLinea mensaje = Left (ErrorFormato numero mensaje)
    guardarCantidad constructor producto valor =
      case readMaybe valor :: Maybe Integer of
        Just cantidad | cantidad >= 0 -> guardar (constructor producto cantidad)
        _ -> errorEnLinea "La cantidad debe ser un entero no negativo"

parsearRegla :: Linea -> Either ErrorFormato (ConOrigen Regla)
parsearRegla (numero, texto) =
  case recortar texto of
    "agotado" -> guardar Agotado
    "stock_bajo" -> guardar StockBajo
    "necesita_reposicion" -> guardar NecesitaReposicion
    nombre -> Left (ErrorFormato numero ("Regla desconocida para inventario: " ++ nombre))
  where
    guardar regla = Right (ConOrigen numero texto regla)

recortar :: String -> String
recortar = dropWhileEnd isSpace . dropWhile isSpace
