import System.IO
import System.Environment
import Control.Monad.State

import Interpreter
import Grammar

main = do
	let a = Atom "a"
	let b = Atom "b"
	let c = Atom "c"
	let d = Atom "d"
	let e = Atom "e"
	let f = Atom "f"
	let g = Atom "g"
	
	let x = Variable "X"
	let y = Variable "Y"
	let z = Variable "Z"
	
	let edge = "edge"
	let twoedge = "twoedge"
	let path = "path"
	
	let clause1 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[a,b]},antecedent = []}
	let clause2 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[a,f]},antecedent = []}
	let clause3 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[f,g]},antecedent = []}
	let clause4 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[b,c]},antecedent = []}
	let clause5 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[g,c]},antecedent = []}
	let clause6 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[f,c]},antecedent = []}
	let clause7 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[f,e]},antecedent = []}
	let clause8 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[c,e]},antecedent = []}
	let clause9 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[c,d]},antecedent = []}
	let clause10 = Clause {consequent = Compound_Term Compound {predicate = edge,args=[e,d]},antecedent = []}
	
	let sub_goal1 = Compound_Term Compound {predicate = edge,args=[x,z]}
	let sub_goal2 = Compound_Term Compound {predicate = edge,args=[z,y]}
	let rule1 = Clause {consequent = Compound_Term Compound {predicate = twoedge,args=[x,y]},antecedent = [sub_goal1,sub_goal2]}
	
	let sub_goal2_1 = Compound_Term Compound {predicate = edge,args=[x,y]}
	let rule2_1 = Clause {consequent = Compound_Term Compound {predicate = path,args=[x,y]},antecedent = [sub_goal2_1]}
	
	let sub_goal2_2 = Compound_Term Compound {predicate = path,args=[z,y]}
	let rule2_2 = Clause {consequent = Compound_Term Compound {predicate = path,args=[x,y]},antecedent = [sub_goal1,sub_goal2_2]}
	
	let program = [clause1,clause2,clause3,clause4,clause5,clause6,clause7,clause8,clause9,clause10,rule1,rule2_1,rule2_2]
	
	let goal = Compound_Term Compound {predicate = twoedge,args=[x,c]}

	let (bindings, _) = runState (satisfy [goal] (Just [])) (ProgState program 0)
	if bindings /= [] then putStrLn $ concat $ map (\ b -> ((showBoundVars b (find_each_var goal)) ++ "\n")) $ bindings
	else putStrLn "false." >> putStrLn ""
	
	
{- 'graph.pl'
edge(a,b).
edge(a,f).
edge(f,g).
edge(b,c).
edge(g,c).
edge(f,c).
edge(f,e).
edge(c,e).
edge(c,d).
edge(e,d)

twoedge(X,Y) :- edge(X,Z),edge(Z,Y).
threeedge(X,Y) :- edge(X,P),edge(P,Z),edge(Z,Y),not(P=Z).

path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z),path(Z,Y).
-}
