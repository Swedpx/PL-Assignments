module HW2 (
    parse, -- reexport for easy terminal use
    foldAndPropagateConstants,
    assignCommonSubexprs,
    reducePoly
) where

import Expression
import Parser
import Data.List

-- the actual tree -> the variable that's going to be substituted -> tree afterwards
evalLeaf :: ExprV -> (String, ExprV) -> ExprV
evalLeaf (Leaf leafVal@(_)) (varName, varVal@(_)) = if (leafVal == (Variable varName)) then varVal
                                                    else (Leaf (leafVal))

evalLeaf (UnaryOperation Minus expr@(_)) (varName, varVal@(_)) = if (expr == (Leaf (Variable varName))) then (UnaryOperation Minus varVal)
                                                                 else (UnaryOperation Minus (evalLeaf expr (varName, varVal)))

evalLeaf (BinaryOperation opr@(_) (expr1@(_)) expr2@(_)) (varName, varVal@(_)) = if (expr1 == (Leaf (Variable varName))) then 

                                                                                        if (expr2 == Leaf (Variable varName)) then (BinaryOperation (opr) (varVal) (varVal) )
                                                                                        else (BinaryOperation (opr) (varVal) (evalLeaf expr2 (varName, varVal)) )

                                                                                 else
                                                                                        if (expr2 == Leaf (Variable varName)) then (BinaryOperation (opr) (evalLeaf expr1 (varName, varVal)) (varVal))
                                                                                        else (BinaryOperation (opr) (evalLeaf expr1 (varName, varVal)) (evalLeaf expr2 (varName, varVal)) )

isConstant :: ExprV -> Bool

isConstant (Leaf (Constant _)) = True
isConstant (Leaf (Variable _)) = False
isConstant (BinaryOperation _ _ _) = False
isConstant (UnaryOperation _ _) = False

reduceTree :: ExprV -> ExprV

--leaf operations.
reduceTree (UnaryOperation Minus (Leaf (Constant val)) ) = Leaf (Constant (-val))
reduceTree (Leaf leafVal@(_)) = Leaf (leafVal)
reduceTree (BinaryOperation opr@(_) (Leaf (Constant val1)) (Leaf (Constant val2))) = if (opr == Times) then Leaf (Constant (val1*val2))
                                                                                     else Leaf (Constant (val1+val2))
--at least one of the leaves is a variable.
reduceTree (BinaryOperation opr@(_) (Leaf leafVal1@(_)) (Leaf leafVal2@(_))) = BinaryOperation (opr) (Leaf (leafVal1)) (Leaf (leafVal2))

reduceTree (UnaryOperation Minus expr@(_)) = (UnaryOperation Minus (reduceTree expr))

reduceTree (BinaryOperation opr@(_) exp1@(_) exp2@(_)) = let lhs = (reduceTree (exp1))
                                                             rhs = (reduceTree (exp2)) in

                                                                 if (isConstant lhs && isConstant rhs) then reduceTree (BinaryOperation opr lhs rhs)
                                                                 else (BinaryOperation opr lhs rhs)

-- the variable that will be substituted -> rest of the list -> substituted list
f :: (String, ExprV) -> [(String, ExprV)] -> [(String, ExprV)]

f (varName ,(Leaf (Constant value))) list = [ ( (fst (old)), (evalLeaf (snd old) ( varName, (Leaf (Constant value)) ) )) | old <- list]
f _ list = list

foldAndPropagateConstants :: [(String, ExprV)] -> [(String, ExprV)]
foldAndPropagateConstants [] = []
foldAndPropagateConstants list = let value = reduceTree (snd (head list)) in
                                    
                                    [ ((fst (head list)), value) ] ++ foldAndPropagateConstants (f ((fst (head list)), value) (tail list) ) 


-- FUNC2 IMPL. STARTS HERE
getLeafOperations :: ExprV -> [ExprV]

getLeafOperations (Leaf leafVal@(_)) = []
getLeafOperations (UnaryOperation Minus (Leaf leafVal@(_)) ) = [UnaryOperation Minus (Leaf (leafVal))]
getLeafOperations (BinaryOperation opr@(_) (Leaf leafVal1@(_)) (Leaf leafVal2@(_)) ) = [BinaryOperation (opr) (Leaf (leafVal1)) (Leaf (leafVal2))]

getLeafOperations (BinaryOperation topOpr@(_) opr1@(_) opr2@(_)) = (getLeafOperations opr1) ++ (getLeafOperations opr2)
getLeafOperations (UnaryOperation Minus opr@(_)) = getLeafOperations opr


getCommonOpr :: [ExprV] -> [ExprV]

getCommonOpr [] = []
getCommonOpr list = if (elem (head list) (tail list)) then [head list] ++ getCommonOpr (filter (\x -> x /= (head list)) (tail list))
                   else getCommonOpr (tail list)

-- actual tree -> expr to be subst and the new name -> tree afterwards
substituteExprS :: ExprV -> (ExprV, String) -> ExprV

substituteExprS (Leaf val@(_)) (_, _)= Leaf (val) --base case, wanted expr. not found                       

substituteExprS (BinaryOperation opr@(_) expr1@(_) expr2@(_)) (wantedExpr, name) = if (expr1 == wantedExpr) then

                                                                                    if (expr2 == wantedExpr) then (BinaryOperation opr (Leaf (Variable name)) (Leaf (Variable name)))
                                                                                    else (BinaryOperation opr (Leaf (Variable name)) (substituteExprS expr2 (wantedExpr, name)) )

                                                                                  else
                                                                                    
                                                                                    if (expr2 == wantedExpr) then (BinaryOperation opr (substituteExprS expr1 (wantedExpr, name)) (Leaf (Variable name)))
                                                                                    else (BinaryOperation opr (substituteExprS expr1 (wantedExpr, name)) (substituteExprS expr2 (wantedExpr, name)) )

substituteExprS (UnaryOperation Minus expr@(_)) (wantedExpr, name) = if (expr == wantedExpr) then (UnaryOperation Minus (Leaf (Variable name)) )
                                                                     else (UnaryOperation Minus (substituteExprS expr (wantedExpr, name)))

fullyMinimize :: ( [(String, ExprV)], ExprV ) -> Int -> ( [(String, ExprV)], ExprV )

fullyMinimize (list, tree) num = let commonOnes = getCommonOpr (getLeafOperations tree) in

                                  if (commonOnes == []) then (list, tree)
                                  else fullyMinimize ( (list ++ [( ("$"++(show num)) ,(commonOnes!!0) )] ), (substituteExprS tree (commonOnes!!0, "$"++(show num))) ) (num+1)


assignCommonSubexprs :: ExprV -> ([(String, ExprV)], ExprV)
assignCommonSubexprs tree = fullyMinimize ([], tree) 0

-- FUNC3 IMPL. STARTS HERE

degree :: ExprV -> Int

degree (Leaf (Variable _)) = 1
degree (Leaf (Constant _)) = 0

degree (BinaryOperation Plus x1 x2) = maximum [degree (x1), degree (x2)]
degree (BinaryOperation Times x1 x2) = (degree x1)+(degree x2)
degree (UnaryOperation _ x) = degree x

negateX :: ExprV -> ExprV

negateX (Leaf (Constant x)) = Leaf (Constant (-x))
negateX (Leaf x) = (UnaryOperation Minus (Leaf x))

negateX (BinaryOperation Times x y) = BinaryOperation Times (negateX x) y
negateX (BinaryOperation Plus x y) = BinaryOperation Plus (negateX x) (negateX y)
negateX (UnaryOperation _ x) = x

multiply :: ExprV -> ExprV -> ExprV

multiply (Leaf (Constant 1)) x = x
multiply x (Leaf (Constant 1)) = x

multiply (Leaf (Constant (-1))) (Leaf (Variable x)) = UnaryOperation Minus (Leaf (Variable x))
multiply (Leaf (Variable x)) (Leaf (Constant (-1))) = UnaryOperation Minus (Leaf (Variable x))

multiply (Leaf (Constant 0)) x = Leaf (Constant 0)
multiply x (Leaf (Constant 0)) = Leaf (Constant 0)

multiply (Leaf leaf) (UnaryOperation _ (Leaf (Variable x))) = BinaryOperation Times (negateX (Leaf leaf)) (Leaf (Variable x))
multiply (UnaryOperation _ (Leaf (Variable x))) (Leaf leaf) = BinaryOperation Times (negateX (Leaf leaf)) (Leaf (Variable x))

multiply (UnaryOperation _ (Leaf (Variable x))) (UnaryOperation _ (Leaf (Variable _))) = BinaryOperation Times (Leaf (Variable x)) (Leaf (Variable x))

multiply (Leaf (Constant val1)) (Leaf (Constant val2)) = Leaf (Constant (val1*val2))

multiply (Leaf (Constant val)) (Leaf (Variable var)) = BinaryOperation Times (Leaf (Constant val)) (Leaf (Variable var))
multiply (Leaf (Variable var)) (Leaf (Constant val)) = BinaryOperation Times (Leaf (Constant val)) (Leaf (Variable var))
multiply (Leaf (Variable var)) (Leaf (Variable _)) = BinaryOperation Times (Leaf (Variable var)) (Leaf (Variable var))

multiply (Leaf l1) (BinaryOperation opr x1 x2) = if (opr == Plus) then BinaryOperation Plus (multiply (Leaf l1) x1) (multiply (Leaf l1) x2)
                                                 else BinaryOperation Times (multiply (Leaf l1) x1) x2

multiply (BinaryOperation opr x1 x2) (Leaf l3) = if (opr == Plus) then BinaryOperation Plus (multiply x1 (Leaf l3)) (multiply x2 (Leaf l3))
                                                 else BinaryOperation Times (multiply x1 (Leaf l3)) x2


multiply (BinaryOperation opr1 x1 x2) (BinaryOperation opr2 x3 x4) = if (opr1 == Times) then

                                                                        if (opr2 == Times) then
                                                                          if (x2 == x3 && x3 == x4) then BinaryOperation Times (BinaryOperation Times (BinaryOperation Times x1 x2) x3) x4
                                                                          else multiply (multiply x1 x3) (multiply x2 x4)
                                                                        else add (multiply (BinaryOperation Times x1 x2) x3) (multiply (BinaryOperation Times x1 x2) x4)

                                                                     else 

                                                                        if (opr2 == Times) then add (multiply x1 (BinaryOperation Times x3 x4)) (multiply x2 (BinaryOperation Times x3 x4))
                                                                        else add (add (multiply x1 x3) (add (multiply x1 x4) (multiply x2 x3))) (multiply x2 x4)
                                                                        
multiply (BinaryOperation opr x1 x2) (UnaryOperation _ x3) = if (opr == Times) then multiply (negateX (BinaryOperation opr x1 x2)) x3
                                                             else add (negateX (multiply x1 x3)) (negateX (multiply x2 x3))

multiply (UnaryOperation _ x1) (BinaryOperation opr x2 x3) = if (opr == Times) then multiply (negateX (BinaryOperation opr x2 x3)) x1
                                                             else add (negateX (multiply x2 x1)) (negateX (multiply x3 x1))
-- lhs -> rhs -> result
add :: ExprV -> ExprV -> ExprV

--base cases
add (Leaf (Constant val1)) (Leaf (Constant val2)) = Leaf (Constant (val1+val2))
add (Leaf (Constant 0)) x = x
add x (Leaf (Constant 0)) = x

-- leaves under minus is a variable
add (UnaryOperation _ (Leaf x)) (Leaf y) = if (isConstant (Leaf y)) then BinaryOperation Plus (Leaf y) (UnaryOperation Minus (Leaf x))
                                           else Leaf (Constant 0)
add (Leaf y) (UnaryOperation _ (Leaf x)) = if (isConstant (Leaf y)) then BinaryOperation Plus (Leaf y) (UnaryOperation Minus (Leaf x))
                                           else Leaf (Constant 0)
add (UnaryOperation _ (Leaf x)) (UnaryOperation _ (Leaf y)) = BinaryOperation Times (Leaf (Constant (-2))) (Leaf x)

add (Leaf (Constant val)) (Leaf (Variable var)) = BinaryOperation Plus (Leaf (Constant val)) (Leaf (Variable var))
add (Leaf (Variable var)) (Leaf (Constant val)) = BinaryOperation Plus (Leaf (Constant val)) (Leaf (Variable var))
add (Leaf (Variable var)) (Leaf (Variable _)) = BinaryOperation Times (Leaf (Constant 2)) (Leaf (Variable var))

add (Leaf l1) (BinaryOperation opr x1 x2) = if (opr == Times) then

                                                if (isConstant (Leaf l1)) then BinaryOperation Plus (Leaf l1) (BinaryOperation Times x1 x2)
                                                else if (isConstant x1) then BinaryOperation Times (add x1 (Leaf (Constant 1))) x2
                                                else BinaryOperation Plus (Leaf l1) (BinaryOperation Times x1 x2)
                                                              
                                            else

                                                if (isConstant (Leaf l1)) then add (add (Leaf l1) x1) x2
                                                else
                                                  if (degree x2 == 1) then add x1 (add x2 (Leaf l1))
                                                  else add (add x1 (Leaf l1)) x2

add (BinaryOperation opr x1 x2) (Leaf l3)= if (opr == Times) then

                                                if (isConstant (Leaf l3)) then BinaryOperation Plus (Leaf l3) (BinaryOperation Times x1 x2)
                                                else if (isConstant x1) then BinaryOperation Times (add x1 (Leaf (Constant 1))) (Leaf l3)
                                                else BinaryOperation Plus (Leaf l3) (BinaryOperation Times x1 x2)
                                                              
                                           else

                                                if (isConstant (Leaf l3)) then add (add x1 (Leaf l3)) x2
                                                else
                                                  if (degree x2 == 1) then add x1 (add x2 (Leaf l3))
                                                  else add (add x1 (Leaf l3)) x2

add (BinaryOperation opr1 x1 x2) (BinaryOperation opr2 x3 x4) = if (opr1 == Plus) then

                                                                    if (opr2 == Plus) then
                                                                        -- sıralamak gerekebilir.
                                                                      if ((degree x1 >= degree x3) || (degree x2 >= degree x4)) then add (add x1 x3) (add x2 x4)
                                                                      else if ((degree x1 >= degree x4) || (degree x2 >= degree x3)) then add (add x1 x4) (add x2 x3)
                                                                      else add (add (add x1 x2) x3) x4
                                                                    else
                                                                      if (degree x1 >= degree (BinaryOperation Times x3 x4)) then add (add x1 (BinaryOperation Times x3 x4)) x2
                                                                      else if (degree x2 >= degree (BinaryOperation Times x3 x4)) then add x1 (add x2 (BinaryOperation Times x3 x4))
                                                                      else BinaryOperation Plus (BinaryOperation Plus x1 x2) (BinaryOperation Times x3 x4)

                                                                else
                                                                    if (opr2 == Plus) then
                                                                      if (degree x3 >= degree (BinaryOperation Times x1 x2)) then add (add x3 (BinaryOperation Times x1 x2)) x4
                                                                      else if (degree x4 >= degree (BinaryOperation Times x1 x2)) then add x3 (add x4 (BinaryOperation Times x1 x2))
                                                                      else BinaryOperation Plus (BinaryOperation Plus x3 x4) (BinaryOperation Times x1 x2)
                                                                    else
                                                                      if (degree (BinaryOperation Times x1 x2) >= degree (BinaryOperation Times x3 x4)) then multiply (add x1 x3) x2
                                                                      else 
                                                                        if (degree (BinaryOperation Times x1 x2) > degree (BinaryOperation Times x3 x4)) then BinaryOperation Plus (BinaryOperation Times x3 x4) (BinaryOperation Times x1 x2)
                                                                        else BinaryOperation Plus (BinaryOperation Times x1 x2) (BinaryOperation Times x3 x4)

add (BinaryOperation opr x1 x2) (UnaryOperation _ x3) = if (opr == Plus) then

                                                            if (degree x2 == 1) then add x1 (add x2 (UnaryOperation Minus x3))
                                                            else add (add x1 (UnaryOperation Minus x3)) x2
                                                        else
                                                            if (isConstant x1) then multiply (add x1 (Leaf (Constant (-1)))) x2
                                                            else BinaryOperation Plus (UnaryOperation Minus x3) (BinaryOperation Times x1 x2)

add (UnaryOperation _ x1) (BinaryOperation opr x2 x3) = if (opr == Plus) then

                                                            if (degree x3 == 1) then add x2 (add x3 (UnaryOperation Minus x1))
                                                            else add (add (UnaryOperation Minus x1) x2) x3
                                                        else
                                                            if (isConstant x2) then multiply (add x2 (Leaf (Constant (-1)))) x3
                                                            else BinaryOperation Plus (UnaryOperation Minus x1) (BinaryOperation Times x2 x3)
reducePoly :: ExprV -> ExprV
reducePoly (Leaf x) = Leaf x

reducePoly (UnaryOperation Minus expr) = negateX (reducePoly expr)
reducePoly (BinaryOperation Plus expr1 expr2) = add (reducePoly expr1) (reducePoly expr2)
reducePoly (BinaryOperation Times expr1 expr2) = multiply (reducePoly expr1) (reducePoly expr2)

-- an extra dummy variable, so as to not crash the GUI
notImpl :: ExprV
notImpl = Leaf $ Variable "Not Implemented"

