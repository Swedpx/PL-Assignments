module Mini1 (
    gridMap,
    gridMapIf,
    evalExpr,
    getVars,
    evalDeriv,
    parse -- reexported to allow use
    ) where

import Expression
import Parser
import Data.List
-- Do not modify the module declaration and imports above!
-- Also do not change the function signatures and do not
-- remove the dummy implementations of the functions if
-- you want your code to compile.

-- Feel free to import anything else here

-- first func.impl.
-- gridMap (20 points), map function over grid elements
gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap f [] = []
gridMap f list = [(map f (head list))] ++ gridMap f (tail list)

-- second func. impl.
mapSingleList :: (a -> Bool) -> (a -> a) -> [a] -> [a]

mapSingleList pred f [] = []
mapSingleList pred f list = if (pred (head list)) then [f (head list)] ++ mapSingleList pred f (tail list)
                            else [(head list)] ++ mapSingleList pred f (tail list)

-- gridMapIf (20 points), map functions over grid elements 
-- that satisfy the predicate provided as the first arg.
gridMapIf :: (a -> Bool) -> (a -> a) -> [[a]] -> [[a]]
gridMapIf predicate f [] = []
gridMapIf predicate f list = [mapSingleList predicate f (head list)] ++ gridMapIf predicate f (tail list)

--third func. impl.

-- the actual tree -> the variable that's going to be substituted -> tree afterwards
evalLeaf :: ExprV -> (String, ExprV) -> ExprV
evalLeaf (Leaf leafVal (varName, varVal) = if (leafVal == (Variable varName)) then varVal
                                           else (Leaf (leafVal))

evalLeaf (UnaryOperation _ expr (varName, varVal) = if (expr == (Leaf (Variable varName))) then (UnaryOperation Minus varVal)
                                                    else (UnaryOperation Minus (evalLeaf expr (varName, varVal)))

evalLeaf (BinaryOperation opr (expr1 expr2 (varName, varVal) = if (expr1 == (Leaf (Variable varName))) then 

                                                                    if (expr2 == Leaf (Variable varName)) then (BinaryOperation (opr) (varVal) (varVal) )
                                                                    else (BinaryOperation (opr) (varVal) (evalLeaf expr2 (varName, varVal)) )

                                                               else
                                                                    if (expr2 == Leaf (Variable varName)) then (BinaryOperation (opr) (evalLeaf expr1 (varName, varVal)) (varVal))
                                                                    else (BinaryOperation (opr) (evalLeaf expr1 (varName, varVal)) (evalLeaf expr2 (varName, varVal)) )
-- all nodes are known to be constants.
evalTree :: ExprV -> Int

evalTree (Leaf (Constant x)) = x
evalTree (BinaryOperation opr x y) = if (opr == Plus) then (evalTree x) + (evalTree y) else (evalTree x) * (evalTree y)
evalTree (UnaryOperation Minus x) = -(evalTree x)

-- evalExpr (20 points), evaluate the expression by
-- substituting (var, value) pairs given in the first arg.
evalExpr :: [(String, Int)] -> ExprV -> Int
evalExpr [] expr = evalTree expr
evalExpr list expr = let varName = fst (list!!0)
                         varValue = snd (list!!0) in evalExpr (tail list) (evalLeaf expr (varName, Leaf (Constant varValue)))

--fourth func. impl.
getAll :: ExprV -> [String]
getAll (Leaf (Constant _)) = []
getAll (Leaf (Variable x)) = [x]
getAll (UnaryOperation _ x) = getAll x
getAll  (BinaryOperation _ x y) = let lhs = getAll x
                                      rhs = getAll y in lhs ++ rhs

onlyUniques :: [String] -> [String]

onlyUniques [] = []
onlyUniques list = [head list] ++ onlyUniques (filter (\x -> x /= (head list)) (tail list))


-- getVars (20 points), return the variables contained
-- in the expression in a list (ordered, no duplicates)
getVars :: ExprV -> [String]
getVars expr = sort (onlyUniques (getAll expr))
                                          
--fifth func. impl.

derivate :: ExprV -> String -> ExprV

derivate (Leaf (Constant _)) _ = Leaf (Constant 0)
derivate (Leaf (Variable name)) der = if (name==der) then Leaf (Constant 1) else Leaf (Constant 0)

derivate (UnaryOperation _ expr) der = UnaryOperation Minus (derivate expr der)
derivate (BinaryOperation opr lhs rhs) der = if (opr == Plus) then BinaryOperation Plus (derivate lhs der) (derivate rhs der)
                                             else BinaryOperation Plus (BinaryOperation Times lhs (derivate rhs der)) (BinaryOperation Times rhs (derivate lhs der))

replaceAllVars :: ExprV -> [(String, Int)] -> ExprV

replaceAllVars expr [] = expr
replaceAllVars expr list = replaceAllVars (evalLeaf expr (fst (head list), Leaf (Constant (snd (head list))))) (tail list)

-- evalDeriv (20 points), evaluate the first derivative
-- with respect to the variable given in the second
-- arg. using (var, value) pairs given in the first arg.
evalDeriv :: [(String, Int)] -> String -> ExprV -> Int

evalDeriv list der expr = evalTree (replaceAllVars (derivate expr der) list)

-- Looks like that's all! 
