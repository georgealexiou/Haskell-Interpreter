{-# LANGUAGE ScopedTypeVariables #-}
import Grammar

-- data Lines = If Line Lines | IfElse Line Lines Lines 
--            | Loop Line Lines
--            | IntDeclareAssign String Line 
--            | IntDeclare String 
--            | BoolDeclareAssign String Line 
--            | BoolDeclare String 
--            | ListDeclareAssign String Line 
--            | ListDeclare String
--            | FloatDeclareAssign String Line 
--            | FloatDeclare String  
--            | ListsDeclareAssign String Line 
--            | ListsDeclare String 
--            | VarAssign String Line
--            | Print String
--            |  Append String Line | Pop String 
--            | LinesSequencing Lines Lines
--            deriving (Show,Eq)

-- data Line = Int Int | Var String |  TTrue | TFalse | TInput
--           | Plus Line Line 
--           | Minus Line Line 
--           | Times Line Line 
--           | Divide Line Line 
--           | Mod Line Line 
--           | Div Line Line  
--           | GreaterEquals Line Line
--           | LessEquals Line Line 
--           | Greater Line Line 
--           | Less Line Line 
--           | Equals Line Line 
--           | NotEquals Line Line 
--           | Or Line Line
--           | And Line Line 
--           | Not Line
--           | Len String
--           | Get String Line | BreakLoop
--           deriving (Show,Eq) 


data Types = TypeInt | TypeBool | TypeList | TypeFloat | TypeLists deriving (Show,Eq)

--Enviroment = Variables
type Environment = [(Types,String,String)] 

--                       list of Kontinuations
--            Controller Frames  Variables
type State = (Lines,[Lines],Environment)

startEvaluation :: Lines -> State
startEvaluation aaa = evaluate (initialiseState $ createListOfLines aaa)

-- takes the result object of happy/parser and makes it into a list of Lines
createListOfLines :: Lines -> [Lines]
createListOfLines (LinesSequencing lines1 lines2) = createListOfLines lines1 ++ createListOfLines lines2
createListOfLines lines1 = [lines1] 

-- initiases the (first) state
initialiseState :: [Lines] -> State
initialiseState liness = (head liness, tail liness, [])

evaluate :: State -> State

-- IntDeclare
evaluate ( (IntDeclare varName), kontinuation, env ) = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[(TypeInt,varName,"0")])

-- BoolDeclare
evaluate ( (BoolDeclare varName), kontinuation, env ) = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[(TypeBool,varName,"False")])


-- ListDeclare
evaluate ( (ListDeclare varName), kontinuation, env ) = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[(TypeList,varName,"[]")] )

-- ListsDeclare
evaluate ( (ListsDeclare varName), kontinuation, env ) = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[(TypeLists,varName,"[]")] )

-- IntDeclareAssign String Line 
evaluate ( (IntDeclareAssign varName value), kontinuation, env ) = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[(TypeInt,varName,calculatedValue)] )
          calculatedValue = show $ calculateInt value

-- BoolDeclareAssign String Line 
evaluate ( (IntDeclareAssign varName value), kontinuation, env ) = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[(TypeInt,varName,calculatedValue)] )
          calculatedValue = show $ calculateInt value
          
-- ListDeclareAssign String Line 

-- ListsDeclareAssign String Line 

-- VarAssign String Line
-- evaluate ( (VarAssign varName linealue), kontinuation, env) = evaluate nextState
evaluate state@( (Print aaa), kontinuation, env) = state
evaluate sda = error "End of stuff"

calculateInt :: Line -> Int
calculateInt (Int a) = a
calculateInt (Plus a b) = calculateInt a + calculateInt b
calculateInt (Minus a b) = calculateInt a - calculateInt b
calculateInt (Times a b) = calculateInt a * calculateInt b
-- calculateInt (Divide a b) = round ( (calculateInt a) / (calculateInt b))
calculateInt (Div a b) = calculateInt a `div` calculateInt b
calculateInt (Mod a b) = calculateInt a `mod` calculateInt b
calculateInt _ = error "Wrong operator used in arithmetic operation" 

calculateBool :: Line -> Bool
calculateBool (TTrue) = True
calculateBool (TFalse) = False
calculateBool (Equals exp1 exp2) = (calculateBool exp1) == (calculateBool exp2)
calculateBool (NotEquals exp1 exp2) = (calculateBool exp1) /= (calculateBool exp2)
calculateBool (Or exp1 exp2) = (calculateBool exp1) || (calculateBool exp2)
calculateBool (And exp1 exp2) = (calculateBool exp1) && (calculateBool exp2)
calculateBool (Not exp1) = not (calculateBool exp1) 
calculateBool (GreaterEquals exp1 exp2) = (calculateInt exp1) >= (calculateInt exp2)
calculateBool (LessEquals exp1 exp2) = (calculateInt exp1) <= (calculateInt exp2)
calculateBool (Greater exp1 exp2) = (calculateInt exp1) > (calculateInt exp2)
calculateBool (Less exp1 exp2) = (calculateInt exp1) < (calculateInt exp2)
calculateBool _ = error "Wrong operator used in boolean expressions"

getType :: (Types,String,String) -> Types
getType (typ,_,_) = typ

getVarName :: (Types,String,String) -> String
getVarName (_,varName,_) = varName

getVarValue :: (Types,String,String) -> String
getVarValue (_,_,varValue) = varValue

checkIfVarExists :: String -> Environment -> Bool
checkIfVarExists varName (var:vars) = getVarName var == varName || checkIfVarExists varName vars
checkIfVarExists _ [] = False


--             Type     Value     Env 
typeChecker :: Types -> String -> Environment -> Bool
typeChecker TypeInt (x:xs) env = (isInt [x]) && (typeChecker TypeInt xs env)
    where isInt value | value == "0" || value == "1" || value == "2" || value == "3" || value == "4"  || value == "5" || value == "6" ||  value == "7" || value == "8" || value == "9" = True
                      | otherwise = False
typeChecker TypeInt [] env = True

typeChecker TypeBool "False" env = True
typeChecker TypeBool "True" env = True
typeChecker TypeBool _ env = False
typeChecker _ _ _ = error "End of type checker"
-- typeChecker TypeList value env = 
-- typeChecker TypeLists value env = 