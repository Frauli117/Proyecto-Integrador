module Main where

main :: IO ()
main = do
  contenido <- readFile "problema.txt"
  putStr contenido
