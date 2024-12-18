module Main where

import GraphFunctions
import Control.Monad.State (runState)

main :: IO ()
main = do
    let graphComp :: NodeGen Graph
        graphComp = do
            graphs <- mapM singleChar_graph ['l','o','l','o','o','l']
            let (g0:g1:g2:g3:g4:g5:_) = graphs
            g7 <- kleeneStar =<< (\x -> orGraphs [x,g4]) =<<  concatGraphs [g2,g3]
            g8 <- plusGraph g5
            concatGraphs [g0,g1,g7,g8]

        (graph,_) = runState graphComp 0

        runningLoop :: IO ()
        runningLoop = do
            word <- getLine
            case word of
              "QUIT" -> return ()
              "LABEL" -> do
                  print $ label graph
                  runningLoop
              otherwise -> do
                  print $ matchGraph graph word
                  runningLoop


    runningLoop

 
  
