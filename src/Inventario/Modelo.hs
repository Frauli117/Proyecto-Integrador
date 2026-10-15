module Inventario.Modelo
  ( Entidad(..), Relacion(..), Regla(..), Seccion(..)
  , ConOrigen(..), Problema(..), ErrorFormato(..)
  ) where


data Entidad
  = Producto String
  | Categoria String
  deriving (Eq, Show)

data Relacion
  = Pertenece String String
  | Existencia String Integer
  | Minimo String Integer
  deriving (Eq, Show)

data Regla
  = Agotado
  | StockBajo
  | NecesitaReposicion
  deriving (Eq, Show)

data Seccion
  = Entidades
  | Relaciones
  | ReglasSolicitadas
  deriving (Eq, Show)

data ConOrigen a = ConOrigen
  { lineaOrigen :: Int
  , textoOrigen :: String
  , dato :: a
  } deriving (Eq, Show)


data Problema = Problema
  { entidades :: [ConOrigen Entidad]
  , relaciones :: [ConOrigen Relacion]
  , reglasSolicitadas :: [ConOrigen Regla]
  , seccionesPresentes :: [Seccion]
  } deriving (Eq, Show)

data ErrorFormato = ErrorFormato
  { lineaError :: Int
  , mensajeError :: String
  } deriving (Eq, Show)
