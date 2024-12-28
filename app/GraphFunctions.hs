module GraphFunctions 
    ( Node(..)
    , Matcher(..)
    , Rule (..)
    , Graph(..)
    , NodeGen
    , matchGraph
    , singleCharGraph
    , anyCharGraph
    , orGraphs
    , kleeneStar
    , plusGraph
    , concatGraphs
    , concatGraphsl
    , orCharGraph
    ) where
        
import Control.Monad.State
import Data.List
import qualified Data.Set as Set



type Queue = [Node]
--Representa los nodos del grafo del automata, el nombre es solo para identificarlos
--si estuviera en un lenguaje no funcional podria usar punteros
newtype Node = Node {name :: String} deriving (Eq,Show,Ord)

--Las transiciones entre nodos que lleva cada regla, Epsilon es una transicion especial asi
--que se maneja de forma separada
data Matcher = Matcher (Char->Bool) String | Epsilon

instance Show Matcher where
    show (Matcher _ s) = s
    show _ = "Epsilon"
    

data Rule = Rule {initial :: Node, matcher :: Matcher, ending :: Node} 

data Graph = Graph {start :: Node, end :: Node, label :: String, rules :: [Rule]} 

--Nuestra instancia de la monada de estado, lleva un contador para los nodos
type NodeGen = State Int

--Identifica si una transicion es de tipo Epsilon
isEpsilon :: Rule -> Bool
isEpsilon (Rule _ Epsilon _) = True
isEpsilon (Rule {}) = False

--Entrega una transicion que solo acepta el caracter entregado
charMatcher :: Char -> Matcher
charMatcher c = Matcher (==c) [c]

--Entrega una transicion que acepta cualquier caracter de la lista
orMatcher :: [Char] -> Matcher
orMatcher chars = Matcher (`elem` chars) chars 

--Importantisima funcion, toma el contador, crea un nodo y sube el contador
makeNode :: String -> NodeGen Node
makeNode prefix = do
    counter <- get
    put (counter + 1)
    return $ Node (prefix ++ (' ' : show counter))

--Las transiciones epsilon son siempre falsas, ya que esta funcion se usa al
--probar caracteres con transiciones que consumen un caracter, de este modo
--las transiciones epsilon nunca consumen un caracter
matchRule :: Rule -> Char -> Bool
matchRule (Rule _ Epsilon _) _ = False
matchRule (Rule _ (Matcher f _) _) c = f c

--Crea una regla que consume un caracter cualquiera, lo acepta siempre
anyChar :: NodeGen Rule
anyChar = do
    node1 <- makeNode "anyNode start"
    node2 <- makeNode "anyNode end"
    return $ Rule node1 (Matcher (const True) ".") node2

--Crea una regla que solo acepta un caracter, consumiendolo
singleChar :: Char -> NodeGen Rule
singleChar c = do
    node1 <- makeNode [c]
    node2 <- makeNode [c]
    return $ Rule node1 (charMatcher c) node2

--Crea una regla que acepta cualquier caracter de una lista
orChar :: [Char] -> NodeGen Rule
orChar chars = do
    node1 <- makeNode $ intersperse '|' chars ++ "start"
    node2 <- makeNode $ intersperse '|' chars ++ "end"
    return $ Rule node1 (orMatcher chars) node2

--Crea un grafo simple que acepta solo un caracter
singleCharGraph :: Char -> NodeGen Graph
singleCharGraph c = do
    rule <- singleChar c
    let node1 = initial rule
        node2 = ending rule
    return $ Graph {start = node1, end = node2, label = [c], rules = [rule]}

orCharGraph :: [Char] -> NodeGen Graph
orCharGraph chars = do
    rule <- orChar chars
    let node1 = initial rule
        node2 = ending rule
    return $ Graph {start = node1, end = node2, label = intersperse '|' chars, rules = [rule]}

--Crea un grafo simple que acepta cualquier caracter
anyCharGraph :: NodeGen Graph
anyCharGraph = do
    rule <- anyChar
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
                      then label graph ++ "+"
                      else "(" ++ label graph ++ ")+"
        newRules = [ Rule newStart Epsilon (start graph)
                   , Rule (end graph) Epsilon (start graph)
                   , Rule (end graph) Epsilon newEnd
                   ]
    return $ Graph {start = newStart, end = newEnd, label = newLabel, rules = newRules ++ rules graph}

--Toma una lista de grafos, y conecta el final de uno con el inicio del otro, aceptando solo palabras
--que pasen todos los grafos
--ej: g0 acepta "a", g1 acepta "b, concatGraphs [g0,g1] solo acepta "ab"
concatGraphsl :: [Graph] -> NodeGen Graph
concatGraphsl graphs = do
    let newLabel = concatMap label graphs
        newRules = zipWith (\graph1 graph2 -> Rule (end graph1) Epsilon (start graph2)) graphs (tail graphs)
        oldRules = concatMap rules graphs
    return $ Graph {start = start $ head graphs, end = end $ last graphs, label = newLabel, rules = newRules ++ oldRules}

concatGraphs :: Graph -> Graph -> NodeGen Graph
concatGraphs g1 g2 = concatGraphsl [g1,g2]
--Toma una lista de grafos y devuelve uno que acepta cualquier palabra que pase al menos uno de los grafos
orGraphs :: [Graph] -> NodeGen Graph
orGraphs graphs = do
    let newLabel = intercalate "|" $ map label graphs
    node1 <- makeNode $ newLabel ++ " start"
    node2 <- makeNode $ newLabel ++ " end"
    let newRules = map (Rule node1 Epsilon . start) graphs ++ map (\graph -> Rule (end graph) Epsilon node2) graphs
        oldRules = concatMap rules graphs
    return $ Graph {start = node1, end = node2, label = newLabel, rules = newRules ++ oldRules}  

--Funcion auxiliar, dado un nodo en un grafo, devuelve todos los nodos alcanzables tras tomar todas las transiciones epsilon posibles
{-followEpsilons :: Graph -> Node -> Set.Set Node
followEpsilons graph node = 
    let resolved = Set.singleton node
        graphRules = map (followEpsilons graph . ending) $ filter (\x -> initial x == node && isEpsilon x) (rules graph)
     in foldr Set.union resolved graphRules 
-}
followEpsilons :: Graph -> Node -> Set.Set Node
followEpsilons graph node = f [node] (Set.singleton node)
    where f [] resolved = resolved
          f (current:xs) resolved = f newQ newS
              where p x = not (Set.member (ending x) resolved) && initial x == current && isEpsilon x
                    rulesList = rules graph
                    newQ = xs ++ map ending (filter p rulesList)
                    newS = Set.union resolved (Set.fromList $ map ending $ filter p rulesList)

--Devuelve todas las reglas que inicien en algun nodo de nuestro conjunto y acepten el caracter dado
matchingRules :: Graph -> Set.Set Node -> Char -> [Rule]
matchingRules graph nodeSet c = filter (\x -> Set.member (initial x) nodeSet && matchRule x c) (rules graph)

--Funcion principal del programa, dada una palabra y un grafo, nos dice si la palabra es aceptada por el grafo
matchGraph :: Graph -> String -> Bool
matchGraph graph word = Set.member (end graph) (foldl (helperFunction graph) states word)
    where states = followEpsilons graph (start graph)
        --funcion auxiliar, dada una lista de nodos, sigue todas las reglas que acepten un caracter especifico y devuelve los nodos resultantes
          helperFunction :: Graph -> Set.Set Node -> Char -> Set.Set Node
          helperFunction g nodes c = foldl (\x y -> Set.union x $ followEpsilons graph $ ending y) Set.empty (matchingRules g nodes c)

