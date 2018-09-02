module Interpreter
	( ProgState (..)
	, Env
	, satisfy
	, showBoundVars
	, find_each_var
	) where

import Grammar

import Data.List
import Data.Maybe
import Control.Monad.State

-- ProgState holds the program and count for renaming the variables. Taken initially zero.
data ProgState = ProgState [Clause] Int

-- Env has all the bindings
type Env =  Maybe [(Term,Term)]

-- Unification Algorithm
unify :: Term -> Term -> Env
unify (Atom a) (Atom b) = if a == b then Just [] else Nothing
unify (Number m) (Number n) = if m == n then Just [] else Nothing
unify (Variable x) term = Just [ (Variable x, term) ]
unify term (Variable x) = Just [ (Variable x, term) ]
unify (Compound_Term Compound {predicate = pred1, args = args1}) (Compound_Term Compound {predicate = pred2, args = args2}) =
	if (pred1 == pred2 && (length args1) == (length args2))
		then unify_args args1 args2
	else Nothing
unify _ _ = Nothing 

unify_args :: [Term] -> [Term] -> Env
unify_args [] [] = Just []
unify_args (arg1:args1) (arg2:args2) =
	merge_env (unify arg1 arg2) (unify_args args1 args2)
	
-- Tries to satisfy the given goal with the given Env and find the respective variables
satisfy :: [Goal] -> Env -> State ProgState [Env]
-- ps is ProgState
satisfy [] env = state $ \ ps  -> ([env], ps)
satisfy (goal:goals) env' = state $ \ ps@(ProgState clauses _) -> runState (do
	renamedClauses <- rename_all clauses
	unifs <- find_unifications goal renamedClauses
	let unifs' = filter (isJust . snd) unifs
	newEnvs <- mapM (\x -> x) [ (satisfy ((replace_all' ant env) ++ (replace_all' goals env)) (merge_env env' env)) | (clause@(Clause cons ant),env) <- unifs' ]
	return $ concat newEnvs) ps
	
-- Finding all the possible unifications
find_unifications :: Goal -> [Clause] -> State ProgState [(Clause, Env)]
find_unifications goal clauses = mapM (unify_goal_nd_clause goal) clauses

-- Unifying each clause with every goal
unify_goal_nd_clause :: Goal -> Clause -> State ProgState (Clause, Env)
unify_goal_nd_clause goal clause@(Clause cons ant) = state $ \ ps -> ((clause, (unify goal cons)), ps)

-- To show the variables bound to their values
showBoundVars :: Env -> [String] -> String
showBoundVars Nothing _ = "false."
showBoundVars _ [] = "true."
showBoundVars binding var_names = intercalate ", " $ map (\ var -> var ++ " = " ++ (show (showBoundVar binding (Variable var)))) var_names

-- To show each variable bound to its value
showBoundVar :: Env -> Term -> Term
showBoundVar binding term = if new_term == term then term
                            else showBoundVar binding new_term
                            where new_term = replace' term binding
               
-- Helper Functions

-- Helper function to merge the environments
merge_env :: Env -> Env -> Env
merge_env _ Nothing = Nothing
merge_env Nothing _ = Nothing
merge_env (Just e1) (Just e2) = Just (e1 ++ e2)

-- Helper function to rename the variables of the clause in the knowledge base to avoid name clashes and assuming that variables doesnot start with '_' or like X_n where n is digit
rename_all :: [Clause] -> State ProgState [Clause]
rename_all clauses = mapM (rename_each) clauses

-- Helper function for rename_all to rename each variable
rename_each :: Clause -> State ProgState Clause
rename_each clause@(Clause cons ant) =do
	renamed_env <- Just <$> mapM (assign_newname) old_varnames
	cons' <- replace cons renamed_env
	ant' <- replace_all ant renamed_env
	return $ Clause cons' ant'
		where old_varnames = find_var (cons:ant)
		
-- Helper function to assign new name to each variable
assign_newname :: String -> State ProgState (Term,Term)
assign_newname name = state $ \(ProgState clauses counter) ->
		((Variable name, Variable (name ++ "_" ++ (show counter))), (ProgState clauses (counter + 1)))
		
-- Helper function to replace each variable with its new name
replace :: Term -> Env -> State ProgState Term
replace term env = state $ \ ps -> (replace' term env, ps)

-- Helper function to check the actual variable name with new variable name
replace' :: Term -> Env -> Term
replace' term env = if term == term' then term
		    else replace' term' env
		    where term' = replace'' term env
		    
-- Helper function which actually replace the variable with its new name
replace'' :: Term -> Env -> Term
replace'' term Nothing = term
replace'' term (Just []) = term
replace'' (Variable x) (Just ((Variable x',y):env')) =
		if x == x' then y
		else replace'' (Variable x) (Just env')
replace'' (Compound_Term Compound {predicate = pred, args = args}) env = Compound_Term Compound {predicate = pred, args = new_args } where
	new_args = (replace_all' args env)
replace'' term _ = term

-- Helper function to replace all the variables from the antecedent of the clause
replace_all :: [Term] -> Env -> State ProgState [Term]
replace_all terms env = state $ \ ps -> (replace_all' terms env, ps)

-- Helper function which ensures that all the variables are replaced with their new names
replace_all' :: [Term] -> Env -> [Term]
replace_all' [] _ = []
replace_all' terms env = map (`replace'` env) terms

-- Helper function to gather all the variables from all the terms.
find_var :: [Term] ->  [String]
find_var [] = []
find_var (term:terms) = union (find_each_var term) (find_var terms)

-- Helper function to gather all the variables in each term.
find_each_var :: Term -> [String]
find_each_var (Variable x) = [x]
find_each_var (Compound_Term Compound {predicate = pred, args = args}) = (find_var args)
find_each_var _ = []
