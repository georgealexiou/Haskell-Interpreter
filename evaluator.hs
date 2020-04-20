{-# LANGUAGE ScopedTypeVariables #-}
module Evaluator where
import Grammar

data Types = TypeInt Int | TypeBool Bool | TypeList [Int] | TypeFloat Int | TypeLists [[Int]] deriving (Show,Eq)

--Enviroment = Variables
type Environment = [(Types,String)] 

--                       list of Kontinuations
--            Controller Frames  Variables
type State = (Lines,[Lines],Environment)

finalise :: State -> String
finalise ( (Print varName), kontinuation, env )  = prettyPrint var where
  var = fst (getVarfromEnv varName env) 

startEvaluation :: Lines -> String
startEvaluation aaa = finalise $ evaluate (initialiseState $ createListOfLines aaa)

startEvaluationWithInput :: Lines -> [[Int]] -> String
startEvaluationWithInput aaa liness = finalise $ evaluate (initialiseStateWithInput (createListOfLines aaa) liness)

-- takes the result object of happy/parser and makes it into a list of Lines
createListOfLines :: Lines -> [Lines]
createListOfLines (LinesSequencing lines1 lines2) = createListOfLines lines1 ++ createListOfLines lines2
createListOfLines lines1 = [lines1] 

-- initiases the (first) state
initialiseState :: [Lines] -> State
initialiseState liness = (head liness, tail liness, [])

-- initiases the (first) state
initialiseStateWithInput :: [Lines] -> [[Int]] -> State
initialiseStateWithInput liness listss = (head liness, tail liness, [((TypeLists listss),"Input")])

evaluate :: State -> State

-- IntDeclare
evaluate ( (IntDeclare varName), kontinuation, env ) 
    | checkIfVarExists varName env = error ("Variable: " ++ varName ++ " already exists")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeInt 0) ,varName)])

-- BoolDeclare
evaluate ( (BoolDeclare varName), kontinuation, env ) 
    | checkIfVarExists varName env = error ("Variable: " ++ varName ++ " already exists")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeBool False),varName) ])


-- ListDeclare
evaluate ( (ListDeclare varName), kontinuation, env ) 
    | checkIfVarExists varName env = error ("Variable: " ++ varName ++ " already exists")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeList []) ,varName)] )

-- ListsDeclare
evaluate ( (ListsDeclare varName), kontinuation, env ) 
    | checkIfVarExists varName env = error ("Variable: " ++ varName ++ " already exists")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeLists []) ,varName)] )

-- IntDeclareAssign String Line 
evaluate ( (IntDeclareAssign varName value), kontinuation, env ) 
    | checkIfVarExists varName env = error ("Variable: " ++ varName ++ " already exists")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeInt calculatedValue) ,varName)] )
          calculatedValue = calculateInt value env

-- BoolDeclareAssign String Line 
evaluate ( (BoolDeclareAssign varName value), kontinuation, env ) 
    | checkIfVarExists varName env = error ("Variable: " ++ varName ++ "already exists")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeBool calculatedValue) ,varName)] )
          calculatedValue = calculateBool value env

-- ListDeclareAssign String Line 
evaluate ( (ListDeclareAssign varName value), kontinuation, env ) 
    | checkIfVarExists varName env = error ("Variable: " ++ varName ++ "already exists")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeList calculatedValue) ,varName)] )
          calculatedValue = calculateList value env

-- ListsDeclareAssign String Line 
evaluate ( (ListDeclareAssign varName value), kontinuation, env ) 
    | checkIfVarExists varName env = error ("Variable: " ++ varName ++ "already exists")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeLists calculatedValue) ,varName)] )
          calculatedValue = calculateLists value env

-- VarAssign String Line
evaluate ( (VarAssign varName value), kontinuation, env ) 
    | not (checkIfVarExists varName env) = error ("Variable: " ++ varName ++ " does not exist")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, newEnv )
          newEnv = newishEnv ++ [(calculatedValue,varName)]                      --gives an env
          calculatedValue = calculateNewValue varType value env
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env   -- gives an env
          varFromEnv = getVarfromEnv varName env        --gives (Type,Str)

--Append String Line
evaluate ((Append varName value), kontinuation, env) 
    | not (checkIfVarExists varName env) = error ("List with name: " ++ varName ++ " does not exist")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, newEnv )
          newEnv = newishEnv ++ [(calculatedValue,varName)]                      --gives an env
          calculatedValue = appendList varType value env
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env   -- gives an env
          varFromEnv = getVarfromEnv varName env        --gives (Type,Str)

--Pop String 
evaluate ((Pop varName), kontinuation, env) 
    | not (checkIfVarExists varName env) = error ("List with name: " ++ varName ++ " does not exist")
    | otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, newEnv )
          newEnv = newishEnv ++ [(calculatedValue,varName)]                      --gives an env
          calculatedValue = popList varType
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env   -- gives an env
          varFromEnv = getVarfromEnv varName env        --gives (Type,Str)

-- If Statement
evaluate ((If condition lines), kontinuation, env) 
    | calculateBool condition env = evaluate ( head (createListOfLines lines), tail (createListOfLines lines) ++ kontinuation, env )
    | otherwise = evaluate (head kontinuation, tail kontinuation, env )

-- IfElse Statement
evaluate ((IfElse condition thenLines elseLines), kontinuation, env) 
    | calculateBool condition env = evaluate ( head (createListOfLines thenLines), tail (createListOfLines thenLines) ++ kontinuation, env )
    | otherwise = evaluate ( head (createListOfLines elseLines), tail (createListOfLines elseLines) ++ kontinuation, env )


-- While Loop
evaluate ((Loop condition lines), kontinuation, env) 
    | calculateBool condition env = evaluate (head (createListOfLines lines), tail (createListOfLines lines) ++ [(Loop condition lines)] ++ kontinuation  , env )
    | otherwise = evaluate (head kontinuation, tail kontinuation, env)

-- evaluate ( (VarAssign varName linealue), kontinuation, env) = evaluate nextState
evaluate state@( (Print varName), kontinuation, env) 
    | not (checkIfVarExists varName env) = error ("Variable with name: " ++ varName ++ " does not exist")
    | otherwise = state

evaluate sda = error (show sda)

prettyPrint :: Types -> String
prettyPrint (TypeList xs) = show xs where
  -- prettyXs = tail $ concatMap (\x -> " ":[(show x)] ) xs 
prettyPrint _ = error "lmao noob"

calculateInt :: Line -> Environment -> Int
calculateInt (Int a) env = a
calculateInt (Negate a) env = (-1) * calculateInt a env
calculateInt (Plus a b) env = calculateInt a env + calculateInt b env
calculateInt (Minus a b) env = calculateInt a env - calculateInt b env
calculateInt (Times a b) env = calculateInt a env * calculateInt b env
-- calculateInt (Divide a b) = round ( (calculateInt a) / (calculateInt b))
calculateInt (Div a b) env = calculateInt a env `div` calculateInt b env
calculateInt (Mod a b) env = calculateInt a env `mod` calculateInt b env
-- calculateInt ()
calculateInt (Var varName) env = value where 
  (TypeInt value) = getVariableValue (TypeInt 0) varName env

calculateInt (Get varName index) env = value where
  value = getFromList var (calculateInt index env)
  var = getVariableValue (TypeList []) varName env

calculateInt (Len varName) env = value where
  value = lenList var
  var = fst $ getVarfromEnv varName env

calculateInt _ _ = error "Wrong operator used in arithmetic operation" 

-- not checking Int == Int
calculateBool :: Line -> Environment -> Bool
calculateBool (TTrue) _ = True
calculateBool (TFalse) _ = False
calculateBool (Equals exp1 exp2) env = (calculateBool exp1 env) == (calculateBool exp2 env)
calculateBool (NotEquals exp1 exp2) env = (calculateBool exp1 env) /= (calculateBool exp2 env)
calculateBool (Or exp1 exp2) env = (calculateBool exp1 env) || (calculateBool exp2 env)
calculateBool (And exp1 exp2) env = (calculateBool exp1 env) && (calculateBool exp2 env)
calculateBool (Not exp1) env = not (calculateBool exp1 env) 

calculateBool (IntEquals exp1 exp2) env = (calculateInt exp1 env) == (calculateInt exp2 env)
calculateBool (GreaterEquals exp1 exp2) env = (calculateInt exp1 env) >= (calculateInt exp2 env)
calculateBool (LessEquals exp1 exp2) env = (calculateInt exp1 env) <= (calculateInt exp2 env)
calculateBool (Greater exp1 exp2) env = (calculateInt exp1 env) > (calculateInt exp2 env)
calculateBool (Less exp1 exp2) env = (calculateInt exp1 env) < (calculateInt exp2 env)
calculateBool (Var varName) env = value where 
  (TypeBool value) = getVariableValue (TypeBool False) varName env
calculateBool _ _ = error "Wrong operator used in boolean expressions"


calculateList :: Line -> Environment -> [Int]
calculateList (Var varName) env = value where 
  (TypeList value) = getVariableValue (TypeList []) varName env
calculateList (Get varName index) env = value where
  value = getFromLists lists (calculateInt index env)
  lists = getVariableValue (TypeLists []) varName env
calculateList _ _ = error "Wrong operator used in list expressions"

calculateLists :: Line -> Environment -> [[Int]]
calculateLists (Var varName) env = value where
  (TypeLists value) = getVariableValue (TypeLists []) varName env
calculateLists _ _ = error "Wrong operator used in lists expressions"


getVariableValue :: Types -> String -> Environment -> Types
getVariableValue (TypeInt _) varName env = checkedVar where
  checkedVar = checkType var (TypeInt 0)  
  var = fst $ getVarfromEnv varName env
getVariableValue (TypeBool _) varName env = checkedVar where
  checkedVar = checkType var (TypeBool False)  
  var = fst $ getVarfromEnv varName env
getVariableValue (TypeList _) varName env = checkedVar where
  checkedVar = checkType var (TypeList [])  
  var = fst $ getVarfromEnv varName env
getVariableValue (TypeLists _) varName env = checkedVar where
  checkedVar = checkType var (TypeLists [])  
  var = fst $ getVarfromEnv varName env

popList :: Types -> Types
popList (TypeList xs) = (TypeList (tail xs) )
popList _ = error "Tried to pop something that is not a List"  

lenList :: Types -> Int
lenList (TypeList xs) = length xs 
lenList (TypeLists xss) = length xss
lenList _ = error "Tried to get the length of a variable that is not List/Lists"

appendList :: Types -> Line -> Environment -> Types
appendList (TypeList xs) expr env = (TypeList (xs++[x]) ) where
  x = calculateInt expr env
appendList _ _ _ = error "Tried to append something that is not a List"

getFromList :: Types -> Int -> Int
getFromList (TypeList xs) i = item where
  item  | i < 0 = error "List out of bounds exception - Used negative number on list get"
        | i >= (length xs) = error "List out of bounds exception - Used number greater than list size"
        | otherwise = xs!!i
getFromList _ _ = error "Tried to get from a variable that is not a List"

getFromLists :: Types -> Int -> [Int]
getFromLists (TypeLists xss) i = item where
  item  | i < 0 = error "Lists out of bounds exception - Used negative number on lists get"
        | i >= (length xss) = error "Lists out of bounds exception - Used number greater than lists size"
        | otherwise = xss!!i
getFromLists _ _ = error "Tried to get from a variable that is not a Lists"


--     thing2Check -> wantedType -> thing2Check
checkType :: Types -> Types -> Types
checkType var@(TypeInt _) (TypeInt _) = var
checkType (TypeBool _) (TypeInt _) = error "Used Boole variable where Int was expected"
checkType (TypeList _) (TypeInt _) = error "Used List variable where Int was expected"
checkType (TypeLists _) (TypeInt _) = error "Used Lists variable where Int was expected"

checkType var@(TypeBool _) (TypeBool _) = var
checkType (TypeInt _) (TypeBool _) = error "Used Int variable where Bool was expected"
checkType (TypeList _) (TypeBool _) = error "Used List variable where Bool was expected"
checkType (TypeLists _) (TypeBool _) = error "Used Lists variable where Bool was expected"

checkType var@(TypeList _) (TypeList _) = var
checkType (TypeInt _) (TypeList _) = error "Used Int variable where List was expected"
checkType (TypeBool _) (TypeList _) = error "Used Bool variable where List was expected"
checkType (TypeLists _) (TypeList _) = error "Used Lists variable where List was expected"

checkType var@(TypeLists _) (TypeLists _) = var
checkType (TypeInt _) (TypeLists _) = error "Used Int variable where Lists was expected"
checkType (TypeBool _) (TypeLists _) = error "Used Bool variable where Lists was expected"
checkType (TypeList _) (TypeLists _) = error "Used List variable where Lists was expected"

checkType _ _ = error "Ise stupid"

calculateNewValue :: Types -> Line -> Environment -> Types
calculateNewValue (TypeInt _) expr env = (TypeInt calcValue) where
  calcValue = calculateInt expr env
calculateNewValue (TypeBool _) expr env = (TypeBool calcValue) where
  calcValue = calculateBool expr env
calculateNewValue (TypeList _) expr env = (TypeList calcValue) where
  calcValue = calculateList expr env
calculateNewValue (TypeLists _) expr env = (TypeLists calcValue) where
  calcValue = calculateLists expr env
calculateNewValue _ _ _ = error "ise malakas"

removeVarFromEnv :: (Types,String) -> Environment -> Environment
removeVarFromEnv variable2Find (var:vars) | var == variable2Find = vars
                                          | otherwise = (var: (removeVarFromEnv variable2Find vars) )
removeVarFromEnv _ [] = []

checkIfVarExists :: String -> Environment -> Bool
checkIfVarExists varName (var:vars) = snd var == varName || checkIfVarExists varName vars
checkIfVarExists _ [] = False

getVarfromEnv :: String -> Environment -> (Types,String)
getVarfromEnv varName env = head results
    where results = filter (\x -> snd x == varName ) env