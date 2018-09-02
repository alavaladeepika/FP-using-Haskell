module Grammar where

import Data.Char
import Data.List

colon_dash = ":-"
lparen = "("
rparen = ")"
comma = ","
dot = "."

data Compound = Compound{
	predicate :: String,
	args :: [Term]
}

data Term = Atom String | Number Int | Variable String | Compound_Term Compound

data Clause = Clause{
	consequent :: Term,
	antecedent :: [Term]
}

type Goal = Term

instance Show Compound where
	show (Compound {predicate = predicate, args = args}) = predicate ++ lparen ++ showArgs args ++ rparen
	
instance Eq Compound where
	(==) (Compound {predicate = a, args = args1}) (Compound {predicate = b, args = args2}) = if a==b then eqArgs args1 args2
												 else False

instance Show Term where
	show (Atom a) 	= a
	show (Number a) = show a
	show (Variable x) = x
	show (Compound_Term t) = show t

instance Eq Term where
	(==) (Atom a) (Atom b)  = a == b
	(==) (Number a) (Number b)  = a == b
	(==) (Variable a) (Variable b)  = a == b
	(==) (Compound_Term a) (Compound_Term b)  = (show a) == (show b)
	(==) _ _ = False

instance Show Clause where 
	show (Clause {consequent = c, antecedent = a}) = show c ++ colon_dash ++ showArgs a

showArgs [] = []
showArgs (x : []) = show x
showArgs (x : xs) = show x ++ comma ++ showArgs xs

eqArgs [] [] = True
eqArgs _ []  = False
eqArgs [] _  = False
eqArgs (x : xs) (y : ys) = (x == y) && eqArgs xs ys
