module Main where

import GraphFunctions
import Control.Monad.State (evalState)
import ParserThing
import System.IO

matchCycle :: Start -> IO ()
matchCycle s = do
    let g = evalState (astToNFA s) 0
    loop g
    where loop g = do
              putStr "Ingrese la cadena a probar: "
              hFlush stdout
              getLine >>= print . matchGraph g
              loop g

main :: IO ()
main = do
    regex <- putStrLn "Ingrese el regex a usar" *> getLine
    let parsedRegex = parse parseRegex regex
    case parsedRegex of 
         Nothing -> do putStrLn "El parser fallo, ingrese denuevo"
                       main
         Just (s,"") -> putStrLn "Regex analizado en su totalidad" *> matchCycle s
         Just (s,str) -> putStrLn ("Regex analizado parcialmente, esto es lo que sobro: " ++ str) *> matchCycle s

    
