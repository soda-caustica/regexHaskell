module Main where

import Control.Monad.State
import Data.List
import qualified Data.Set as Set


--Representa los nodos del grafo del automata, el nombre es solo para identificarlos
--si estuviera en un lenguaje no funcional podria usar punteros
data Node = Node {name :: String} deriving (Eq,Show,Ord)

--Las transiciones entre nodos que lleva cada regla, Epsilon es una transicion especial asi
--que se maneja de forma separada
data Matcher = Matcher (Char->Bool) | Epsilon

data Rule = Rule {initial :: Node, matcher :: Matcher, ending :: Node} 

data Graph = Graph {start :: Node, end :: Node, label :: String, rules :: [Rule]} 

--Nuestra instancia de la monada de estado, lleva un contador para los nodos
type NodeGen = State Int

--Identifica si una transicion es de tipo Epsilon
isEpsilon :: Rule -> Bool
isEpsilon (Rule _ Epsilon _) = True
isEpsilon (Rule _ _ _) = False

--Entrega una transicion que solo acepta el caracter entregado
char_matcher :: Char -> Matcher
char_matcher c = Matcher (\x -> x == c)

--Entrega una transicion que acepta cualquier caracter de la lista
or_matcher :: [Char] -> Matcher
or_matcher chars = Matcher (\x -> x `elem` chars)  

--Importantisima funcion, toma el contador, crea un nodo y sube el contador
makeNode :: String -> NodeGen Node
makeNode prefix = do
    counter <- get
    put (counter + 1)
    return $ Node (prefix ++ (' ' : show counter))

--Las transiciones epsilon son siempre falsas, ya que esta funcion se usa al
--probar caracteres con transiciones que consumen un caracter, de este modo
--las transiciones epsilon nunca consumen un caracter
match_rule :: Rule -> Char -> Bool
match_rule (Rule _ Epsilon _) _ = False
match_rule (Rule _ (Matcher f) _) c = f c

--Crea una regla que consume un caracter cualquiera, lo acepta siempre
any_char :: NodeGen Rule
any_char = do
    node1 <- makeNode "anyNode start"
    node2 <- makeNode "anyNode end"
    return $ Rule node1 (Matcher (\_ -> True)) node2

--Crea una regla que solo acepta un caracter, consumiendolo
single_char :: Char -> NodeGen Rule
single_char c = do
    node1 <- makeNode [c]
    node2 <- makeNode [c]
    return $ Rule node1 (char_matcher c) node2

--Crea una regla que acepta cualquier caracter de una lista
or_char :: [Char] -> NodeGen Rule
or_char chars = do
    node1 <- makeNode $ intersperse '|' (chars) ++ "start"
    node2 <- makeNode $ intersperse '|' (chars) ++ "end"
    return $ Rule node1 (or_matcher chars) node2

--Crea un grafo simple que acepta solo un caracter
singleChar_graph :: Char -> NodeGen Graph
singleChar_graph c = do
    rule <- single_char c
    let node1 = initial rule
        node2 = ending rule
    return $ Graph {start = node1, end = node2, label = [c], rules = [rule]}

--Crea un grafo simple que acepta cualquier caracter
anyChar_graph :: NodeGen Graph
anyChar_graph = do
    rule <- any_char
    let node1 = initial rule
        node2 = ending rule
    return $ Graph {start = node1, end = node2, label = ".", rules = [rule]}

--Toma un grafo y devuelve otro que acepta 0, 1 o varias veces la palabra del grafo original
--ej: g0 acepta "ab", kleeneStar g0 aceptaria "", "ababababab", "ab", etc
kleeneStar :: Graph -> NodeGen Graph
kleeneStar graph = do
    newStart <- makeNode "kleeneStarBegin"
    newEnd <- makeNode "kleeneStarEnd"
    let newLabel = if (length.label) graph == 1 
                      then label graph ++ "*"
                      else "(" ++ label graph ++ ")*"
        newRules = [ Rule newStart Epsilon newEnd
                   , Rule newStart Epsilon (start graph)
                   , Rule (end graph) Epsilon (start graph)
                   , Rule (end graph) Epsilon newEnd
                   ]
    return $ Graph {start = newStart, end = newEnd, label = newLabel, rules = newRules ++ rules graph}

--Lo mismo que kleeneStar, solo que no acepta la cadena vacia
plusGraph :: Graph -> NodeGen Graph
plusGraph graph = do
    newStart <- makeNode "kleeneStarBegin"
    newEnd <- makeNode "kleeneStarEnd"
    let newLabel = if (length.label) graph == 1 
                      then label graph ++ "*"
                      else "(" ++ label graph ++ ")*"
        newRules = [ Rule newStart Epsilon (start graph)
                   , Rule (end graph) Epsilon (start graph)
                   , Rule (end graph) Epsilon newEnd
                   ]
    return $ Graph {start = newStart, end = newEnd, label = newLabel, rules = newRules ++ rules graph}

--Toma una lista de grafos, y conecta el final de uno con el inicio del otro, aceptando solo palabras
--que pasen todos los grafos
--ej: g0 acepta "a", g1 acepta "b, concatGraphs [g0,g1] solo acepta "ab"
concatGraphs :: [Graph] -> NodeGen Graph
concatGraphs graphs = do
    let newLabel = concatMap label graphs
        newRules = zipWith (\graph1 graph2 -> Rule (end graph1) Epsilon (start graph2)) graphs (tail graphs)
        oldRules = concat $ map rules graphs
    return $ Graph {start = (start $ head graphs), end = (end $ last graphs), label = newLabel, rules = newRules ++ oldRules}

--Toma una lista de grafos y devuelve uno que acepta cualquier palabra que pase al menos uno de los grafos
orGraphs :: [Graph] -> NodeGen Graph
orGraphs graphs = do
    let newLabel = concat $ intersperse "|" $ map label graphs
    node1 <- makeNode $ newLabel ++ " start"
    node2 <- makeNode $ newLabel ++ " end"
    let newRules = map (\graph -> Rule node1 Epsilon (start graph)) graphs ++ map (\graph -> Rule (end graph) Epsilon node2) graphs
        oldRules = concat $ map rules graphs
    return $ Graph {start = node1, end = node2, label = newLabel, rules = newRules ++ oldRules}  

--Funcion auxiliar, dado un nodo en un grafo, devuelve todos los nodos alcanzables tras tomar todas las transiciones epsilon posibles
followEpsilons :: Graph -> Node -> Set.Set Node
followEpsilons graph node = 
    let resolved = Set.singleton node
        graphRules = map (followEpsilons graph . ending) $ filter (\x -> initial x == node && isEpsilon x) (rules graph)
     in foldr Set.union resolved graphRules 

--Devuelve todas las reglas que inicien en algun nodo de nuestro conjunto y acepten el caracter dado
matchingRules :: Graph -> Set.Set Node -> Char -> [Rule]
matchingRules graph nodeSet c = filter (\x -> Set.member (initial x) nodeSet && match_rule x c) (rules graph)

--Funcion principal del programa, dada una palabra y un grafo, nos dice si la palabra es aceptada por el grafo
matchGraph :: Graph -> String -> Bool
matchGraph graph word = 
    let states = followEpsilons graph (start graph)
        --funcion auxiliar, dada una lista de nodos, sigue todas las reglas que acepten un caracter especifico y devuelve los nodos resultantes
        helperFunction :: Graph -> Set.Set Node -> Char -> Set.Set Node
        helperFunction g nodes c = foldl (\x y -> Set.union x $ followEpsilons graph $ ending y) Set.empty (matchingRules g nodes c)
    -- por cada letra de la palabra seguimos todas las reglas, obteniendo nuestros nuevos nodos a entregar a la siguiente letra
    -- despues comprobamos si la letra final llega al final de grafo, lo que significaria que nuestra palabra cumple el patron
    in Set.member (end graph) (foldl (helperFunction graph) states word)

main :: IO ()
main = do
    let graphComp :: NodeGen Graph
        graphComp = do
            graphs <- mapM singleChar_graph ['l','o','l','o','o','l','l']
            let (g0:g1:g2:g3:g4:g5:g6:_) = graphs
            g7 <- kleeneStar =<< (\x -> orGraphs [x,g4]) =<<  concatGraphs [g2,g3]
            g8 <- kleeneStar g6
            concatGraphs [g0,g1,g7,g5,g8]

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

 
  
