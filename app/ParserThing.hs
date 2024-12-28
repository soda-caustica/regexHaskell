module ParserThing 
    ( parseRegex
    , astToNFA
    , Parser(..)
    , Start ()
    ) where

import Control.Applicative

import GraphFunctions

newtype Parser a = Parser {parse :: String -> Maybe (a,String) }

instance Show a => Show (Parser a) where
    show _ = "Parser"

instance Functor Parser where
    fmap f p = Parser $ \s -> do
        (x,xs) <- parse p s
        return (f x, xs)

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x,s)
    pf <*> px = Parser $ \s -> do
        (f,s1) <- parse pf s
        (x,s2) <- parse px s1
        return (f x, s2)

instance Monad Parser where
    px >>= f = Parser $ \s -> do
        (x,s') <- parse px s
        parse (f x) s'

instance MonadFail Parser where
    fail _ = Parser $ const Nothing

instance Alternative Parser where
    empty = fail "filler"
    p1 <|> p2 = Parser $ \s ->
        case parse p1 s of
            Just x -> Just x
            Nothing -> parse p2 s

char :: Char -> Parser Char
char c = Parser func
    where func []     = Nothing
          func (x:xs) = if x == c
                          then Just (x,xs)
                          else Nothing

chars :: [Char] -> Parser Char
chars = foldr ((<|>).char) empty

-- EMPIEZA LO DE VERDAD, DEFINAMOS NUESTROS TIPOS

alphabet :: [Char]
alphabet = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

type Character = Char
character :: Parser Character
character = chars alphabet

data Modifier = Kleene | Plus deriving Show
modifier :: Parser Modifier
modifier = fmap f (char '*' <|> char '+')
    where f '*' = Kleene
          f '+' = Plus
          f _ = error "hate hls warnings"

data Item = Item Character | ItemPlus Group
item :: Parser Item
item = Item <$> character <|> ItemPlus <$> group

newtype Group = Group Expression
group :: Parser Group
group = Group <$> (char '(' *> expression <* char ')')

data Term = TermPlus Item Modifier | Term Item
term :: Parser Term
term = TermPlus <$> item <*> modifier <|> Term <$> item

data Expression = ExpressionPlus Term Expression | Expression Term
expression :: Parser Expression
expression = ExpressionPlus <$> term <*> expression <|> Expression <$> term

data Start = StartExpression Expression | StartEpsilon
parseRegex :: Parser Start
parseRegex = StartExpression <$> expression <|> return StartEpsilon

astToNFA :: Start -> NodeGen Graph
astToNFA (StartExpression expr) = expressionToNFA expr
astToNFA StartEpsilon = epsilonGraph

expressionToNFA :: Expression -> NodeGen Graph
expressionToNFA (ExpressionPlus trm expr) = do
    tg <- termToNFA trm
    eg <- expressionToNFA expr
    concatGraphs tg eg
expressionToNFA (Expression trm) = termToNFA trm
    
termToNFA :: Term -> NodeGen Graph
termToNFA (TermPlus itm modif) = do
    ig <- itemToNFA itm
    case modif of
         Kleene -> kleeneStar ig
         Plus -> plusGraph ig
termToNFA (Term itm) = itemToNFA itm

itemToNFA :: Item -> NodeGen Graph
itemToNFA (Item c) = singleCharGraph c
itemToNFA (ItemPlus g) = groupToNFA g

groupToNFA :: Group -> NodeGen Graph
groupToNFA (Group expr) = expressionToNFA expr
