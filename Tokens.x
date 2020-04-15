{ 
module Tokens where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
$white+       ;
  "--".*        ; 
  int              { \s -> TokenTypeInt }
  boolean          { \s -> TokenTypeBool }
  float            { \s -> TokenTypeFloat }
  list             { \s -> TokenTypeList }
  lists            { \s -> TokenTypeLists }
  loop             { \s -> TokenLoop }
  do\:             { \s -> TokenDo }
  if               { \s -> TokenIf}
  endLoop          { \s -> TokenEndLoop }
  endIf            { \s -> TokenEndIf }
  else             { \s -> TokenElse }
  break            { \s -> TokenBreak }
  print            { \s -> TokenPrint }
  input            { \s -> TokenInput}

  \.len            { \s -> TokenListLength }  
  \.append         { \s -> TokenListAppend }
  \.pop            { \s -> TokenListPop }
  \.get            { \s -> TokenListGet }

  \>\=             { \s -> TokenGreaterEquals }
  \<\=             { \s -> TokenLessEquals }
  \>               { \s -> TokenGreater }
  \<               { \s -> TokenLess }
  \=\=             { \s -> TokenEquals }
  \!\=             { \s -> TokenNotEquals}
  \!               { \s -> TokenNot}
  or               { \s -> TokenOR }
  and              { \s -> TokenAND }

  $digit+          { \s -> TokenDigit (read s) }
  \+\+             { \s -> TokenIncrement }
  \=               { \s -> TokenAssign }
  \+               { \s -> TokenPlus }
  \-               { \s -> TokenMinus }
  \*               { \s -> TokenTimes }
  \/               { \s -> TokenDiv }
  mod              { \s -> TokenModulo }
  div              { \s -> TokenIntDiv }

  \(               { \s -> TokenParenthesisOpen }
  \)               { \s -> TokenParenthesisClose }
  \[               { \s -> TokenSquareBracketsOpen }
  \]               { \s -> TokenSquareBracketsClose }

  true             { \s -> TokenTrue }
  false            { \s -> TokenFalse }

  \;               { \s -> TokenEndLine }

  $alpha [$alpha $digit \_ \’]*   { \s -> TokenVar s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
   TokenTypeInt                          |
   TokenTypeBool                         |
   TokenTypeFloat                        |
   TokenTypeList                         |
   TokenTypeLists                        |
   TokenLoop                         |
   TokenDo                           |
   TokenIf                           |
   TokenEndLoop                      |
   TokenEndIf                        |
   TokenElse                         |
   TokenBreak                        |
   TokenPrint                        |
   TokenInput                        |
   TokenListLength                   |
   TokenListAppend                   |
   TokenListPop                      |
   TokenListGet               |
   TokenGreaterEquals                |
   TokenLessEquals                   |
   TokenGreater                      |
   TokenLess                         |
   TokenEquals                       |
   TokenNotEquals                    |
   TokenNot                          |
   TokenDigit Int                    |
   TokenIncrement                    |
   TokenAssign                       |
   TokenPlus                         |
   TokenMinus                        |
   TokenTimes                        |
   TokenDiv                          |
   TokenModulo                       |
   TokenIntDiv                       |
   TokenParenthesisOpen              |
   TokenParenthesisClose             |
   TokenSquareBracketsOpen           |
   TokenSquareBracketsClose          |
   TokenTrue                         |
   TokenFalse                        |
   TokenEndLine                      |
   TokenOR                           |
   TokenAND                          |
   TokenVar String
  deriving (Eq,Show) 

}