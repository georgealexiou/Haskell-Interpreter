{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
$white+       ;
  "--".*        ; 
  int              { tok (\p s -> TokenTypeInt p) }
  boolean          { tok (\p s -> TokenTypeBool p)}
  float            { tok (\p s -> TokenTypeFloat p)}
  list             { tok (\p s -> TokenTypeList p)}
  lists            { tok (\p s -> TokenTypeLists p)}
  loop             { tok (\p s -> TokenLoop p)}
  do\:             { tok (\p s -> TokenDo p)}
  if               { tok (\p s -> TokenIf p)}
  endLoop          { tok (\p s -> TokenEndLoop p)}
  endIf            { tok (\p s -> TokenEndIf p)}
  else             { tok (\p s -> TokenElse p)}
  break            { tok (\p s -> TokenBreak p)}
  print            { tok (\p s -> TokenPrint p)}

  \.len            { tok (\p s -> TokenListLength p)}  
  \.append         { tok (\p s -> TokenListAppend p)}
  \.pop            { tok (\p s -> TokenListPop p)}
  \.get            { tok (\p s -> TokenListGet p)}

  \>\=             { tok (\p s -> TokenGreaterEquals p)}
  \<\=             { tok (\p s -> TokenLessEquals p)}
  \>               { tok (\p s -> TokenGreater p)}
  \<               { tok (\p s -> TokenLess p)}
  \=\=             { tok (\p s -> TokenEquals p)}
  \=\=\=           { tok (\p s -> TokenIntEquals p)}
  \!\=             { tok (\p s -> TokenNotEquals p)}
  \!               { tok (\p s -> TokenNot p)}
  or               { tok (\p s -> TokenOR p)}
  and              { tok (\p s -> TokenAND p)}

  $digit+          { tok (\p s -> TokenDigit p (read s) ) }
  \+\+             { tok (\p s -> TokenIncrement p) }
  \=               { tok (\p s -> TokenAssign p) }
  \+               { tok (\p s -> TokenPlus p) }
  \-               { tok (\p s -> TokenMinus p) }
  \*               { tok (\p s -> TokenTimes p) }
  \/               { tok (\p s -> TokenDiv p) }
  mod              { tok (\p s -> TokenModulo p) }
  div              { tok (\p s -> TokenIntDiv p) }

  \(               { tok (\p s -> TokenParenthesisOpen p) }
  \)               { tok (\p s -> TokenParenthesisClose p) }
  \[               { tok (\p s -> TokenSquareBracketsOpen p) }
  \]               { tok (\p s -> TokenSquareBracketsClose p) }

  true             { tok (\p s -> TokenTrue p) }
  false            { tok (\p s -> TokenFalse p) }

  \;               { tok (\p s -> TokenEndLine p) }

  $alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s) } 

{ 

-- Helper function
tok f p s = f p s

-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
   TokenTypeInt AlexPosn                      |
   TokenTypeBool AlexPosn                     |
   TokenTypeFloat AlexPosn                    |
   TokenTypeList AlexPosn                     |
   TokenTypeLists AlexPosn                    |
   TokenLoop AlexPosn                         |
   TokenDo AlexPosn                           |
   TokenIf AlexPosn                           |
   TokenEndLoop AlexPosn                      |
   TokenEndIf AlexPosn                        |
   TokenElse AlexPosn                         |
   TokenBreak AlexPosn                        |
   TokenPrint AlexPosn                        |
   TokenListLength AlexPosn                   |
   TokenListAppend AlexPosn                   |  
   TokenListPop AlexPosn                      |
   TokenListGet AlexPosn                      |
   TokenGreaterEquals AlexPosn                |
   TokenLessEquals AlexPosn                   |
   TokenGreater AlexPosn                      |
   TokenLess AlexPosn                         |
   TokenEquals AlexPosn                       |
   TokenIntEquals AlexPosn                    |
   TokenNotEquals AlexPosn                    |
   TokenNot AlexPosn                          |
   TokenDigit AlexPosn Int                    |
   TokenIncrement AlexPosn                    |
   TokenAssign AlexPosn                       |
   TokenPlus AlexPosn                         |
   TokenMinus AlexPosn                        |
   TokenTimes AlexPosn                        |
   TokenDiv AlexPosn                          |
   TokenModulo AlexPosn                       |
   TokenIntDiv AlexPosn                       |
   TokenParenthesisOpen AlexPosn              |
   TokenParenthesisClose AlexPosn             |
   TokenSquareBracketsOpen AlexPosn           |
   TokenSquareBracketsClose AlexPosn          |
   TokenTrue AlexPosn                         |   
   TokenFalse AlexPosn                        |
   TokenEndLine AlexPosn                      |
   TokenOR AlexPosn                           |
   TokenAND AlexPosn                          |
   TokenVar AlexPosn String
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenDigit (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenTypeInt (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenTypeFloat (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenTypeList (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenTypeLists (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenLoop (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenDo (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenEndLoop (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenEndIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenBreak (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenPrint (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenListLength (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenListAppend (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenListPop (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenListGet (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenGreaterEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenLessEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenGreater (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenLess (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenIntEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenNotEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenNot (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenIncrement (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenAssign (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenTimes (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenDiv (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenModulo (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenIntDiv (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenParenthesisOpen (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenParenthesisClose (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenSquareBracketsOpen (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenSquareBracketsClose (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenTrue (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenFalse (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenEndLine (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenOR (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenAND (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 


}