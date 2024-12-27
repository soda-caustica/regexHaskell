module Main where

import GraphFunctions (matchGraph,Graph)
import Control.Monad.State (runState)
import ParserThing

loopK :: Graph -> IO ()
loopK g = do
    word <- getLine
    print $ matchGraph g word
    loopK g


main :: IO ()
main = do
    regex <- getLine
    let r = parse parseRegex regex 
        Just (s,_) = r
        (g,_) = runState (astToNFA s) 0
    loopK g
  
