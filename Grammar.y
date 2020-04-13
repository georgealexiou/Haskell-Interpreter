{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    Int           { TokenInt }
    Bool           { TokenBool }
    Float          { TokenFloat }
    List           { TokenList }
    Lists          { TokenLists }
    loop           { TokenLoop }
    do             { TokenDo }
    if             { TokenIf }
    endLoop        { TokenEndLoop }
    endIf          { TokenEndIf }
    else           { TokenElse }
    break          { TokenBreak }
    print          { TokenPrint }
    input          { TokenInput }
    len            { TokenListLength }
    append         { TokenListAppend }
    pop            { TokenListPop }
    get            { TokenListGetElement }
    '>='           { TokenGreaterEquals }
    '<='           { TokenLessEquals }
    '>'            { TokenGreater }
    '<'            { TokenLess }
    '='            { TokenEquals }
    '!='           { TokenNotEquals }
    '!'            { TokenNot }
    '++'           { TokenAssign }
    '='            { TokenAssign }
    var            { TokenVar $$ }
    int            { TokenDigit $$ }
    '+'            { TokenPlus }
    '-'            { TokenMinus }
    '*'            { TokenTimes }
    '/'            { TokenDiv }
    div            { TokenIntDiv }
    mod            { TokenModulo }
    '('            { TokenParenthesisOpen }
    ')'            { TokenParenthesisClose }
    '['            { TokenSquareBracketsOpen }
    ']'            { TokenSquareBracketsClose }
    true           { TokenTrue }
    false          { TokenFalse }
    ';'            { TokenEndLine }


%right in 
%left '+' '-' 
%left '*' '/' 
%left NEG 
%% 
Exp : let var '=' Exp in Exp { Let $2 $4 $6 } 
    | Exp '+' Exp            { Plus $1 $3 } 
    | Exp '-' Exp            { Minus $1 $3 } 
    | Exp '*' Exp            { Times $1 $3 } 
    | Exp '/' Exp            { Div $1 $3 } 
    | '(' Exp ')'            { $2 } 
    | '-' Exp %prec NEG      { Negate $2 } 
    | int                    { Int $1 } 
    | var                    { Var $1 } 
    
{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 
data Exp = Let String Exp Exp 
         | Plus Exp Exp 
         | Minus Exp Exp 
         | Times Exp Exp 
         | Div Exp Exp 
         | Negate Exp
         | Int Int 
         | Var String 
         deriving Show 
} 