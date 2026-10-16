module Inventario.Parser (Linea, separarSecciones) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Inventario.Modelo (Seccion(..))


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
    limpio = dropWhileEnd isSpace (dropWhile isSpace texto)

reconocerSeccion :: String -> Maybe Seccion
reconocerSeccion "[ENTIDADES]" = Just Entidades
reconocerSeccion "[RELACIONES]" = Just Relaciones
reconocerSeccion "[REGLAS_SOLICITADAS]" = Just ReglasSolicitadas
reconocerSeccion _ = Nothing
