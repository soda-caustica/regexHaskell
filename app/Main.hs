module Main where

import GraphFunctions
import Control.Monad.State (evalState)
import ParserThing

loopK :: Graph -> IO ()
loopK g = do
    print $ label g
    word <- getLine
    print $ matchGraph g word
    loopK g


main :: IO ()
main = do
    let gc = kleeneStar =<< kleeneStar =<< singleCharGraph 'a'
    print 3
    -- Prueba con algunas cadenas de entrada directamente
    print $ matchGraph (evalState gc 0) "a"   -- Debería dar True
    print $ matchGraph (evalState gc 0) "aa"  -- Debería dar True
    print $ matchGraph (evalState gc 0) "aaa" -- Debería dar True
    print $ matchGraph (evalState gc 0) "b"   -- Debería dar False
    print 4
    loopK (evalState gc 0) 
