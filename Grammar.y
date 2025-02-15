{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token
    break          { TokenBreak _ }
    len            { TokenListLength _ }
    get            { TokenListGet _ }
    '++'           { TokenIncrement _ }
    append         { TokenListAppend _ }
    pop            { TokenListPop _ }
    '='            { TokenAssign _}
    TypeInt        { TokenTypeInt _}
    TypeBool           { TokenTypeBool _}
    TypeFloat          { TokenTypeFloat _}
    TypeList           { TokenTypeList _}
    TypeLists          { TokenTypeLists _}
    print          { TokenPrint _}
	loop           { TokenLoop _}
    do             { TokenDo _}
    if             { TokenIf _}
    endLoop        { TokenEndLoop _}
    endIf          { TokenEndIf _}
    else           { TokenElse _}
    div            { TokenIntDiv _}
    mod            { TokenModulo _}
    '>='           { TokenGreaterEquals _}
    '<='           { TokenLessEquals _}
    '>'            { TokenGreater _}
    '!='           { TokenNotEquals _}
    '!'            { TokenNot _}
    OR             { TokenOR _}
    AND            { TokenAND _}
    '<'            { TokenLess _}
    '=='           { TokenEquals _ }
    '==='          { TokenIntEquals _ }
    varName        { TokenVar _ $$ }
    int            { TokenDigit _ $$ }
    '+'            { TokenPlus _}
    '-'            { TokenMinus _}
    '*'            { TokenTimes _}
    '/'            { TokenDiv _}
    '('            { TokenParenthesisOpen _}
    ')'            { TokenParenthesisClose _}
    true           { TokenTrue _}
    false          { TokenFalse _}
    ';'            { TokenEndLine _}

%left ';'
%nonassoc '(' ')' 
%nonassoc get len
%nonassoc print
%nonassoc '=' '++'
%nonassoc append pop
%nonassoc TypeInt TypeBool TypeFloat TypeList TypeLists
%nonassoc if endIf loop endLoop print do
%left AND OR
%left '!=' '=='
%right '!'
%nonassoc '<=' '>=' '>' '<' '==='
%left '++'
%left '+' '-'
%left '*' '/'
%left mod div
%nonassoc int true false varName
%left NEG

%%
Lines : if '(' Line ')' Lines endIf             { If $3 $5 }
      | if '(' Line ')' Lines else Lines endIf  { IfElse $3 $5  $7}
      | loop '(' Line ')' do Lines endLoop      { Loop $3 $6 }
      | print varName                           { Print $2 }
      | TypeInt varName                         { IntDeclare $2 }
      | TypeInt varName '=' Line                { IntDeclareAssign $2 $4 }
      | TypeBool varName                        { BoolDeclare $2 }
      | TypeBool varName '=' Line               { BoolDeclareAssign $2 $4 }
      | TypeFloat varName                       { FloatDeclare $2 }
      | TypeFloat varName '=' Line              { FloatDeclareAssign $2 $4 }
      | TypeList varName                        { ListDeclare $2 }
      | TypeList varName '=' Line               { ListDeclareAssign $2 $4 }
      | TypeLists varName                       { ListsDeclare $2 }
      | TypeLists varName '=' Line              { ListsDeclareAssign $2 $4 }
      | varName '=' Line                        { VarAssign $1 $3 }
      | varName append '(' Line ')'             { Append $1 $4 }
      | varName pop                             { Pop $1 }
      | varName '++'                            { VarAssign $1 (Plus (Var $1) (Int 1)) }
      | Lines ';' Lines                         { LinesSequencing $1 $3 }
      | Lines ';'                               { $1 }

Line : 
       Line mod Line            { Mod $1 $3 } 
     | Line div Line            { Div $1 $3 }
     | Line '>=' Line           { GreaterEquals $1 $3 } 
     | Line '<=' Line           { LessEquals $1 $3 } 
     | Line '>' Line            { Greater $1 $3 } 
     | Line '!=' Line           { NotEquals $1 $3 }
     | Line AND Line            { And $1 $3 }
     | Line OR Line             { Or $1 $3 }
     | '!' Line                 { Not $2 } 
     | Line '+' Line            { Plus $1 $3 } 
     | Line '-' Line            { Minus $1 $3 } 
     | Line '*' Line            { Times $1 $3 } 
     | Line '/' Line            { Divide $1 $3 }
     | Line '<' Line            { Less $1 $3 }
     | Line '==' Line           { Equals $1 $3 }
     | Line '===' Line          { IntEquals $1 $3 }
     | '(' Line ')'             { $2 } 
     | true                     { TTrue }
     | false                    { TFalse }
     | int                      { Int $1 } 
     | varName                  { Var $1 }
     | varName len              { Len $1 }
     | varName get '(' Line ')' { Get $1 $4 }
     | break                    { BreakLoop }
     | '-' Line %prec NEG       { Negate $2 } 
    
{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Lines = If Line Lines | IfElse Line Lines Lines 
           | Loop Line Lines
           | IntDeclareAssign String Line 
           | IntDeclare String 
		   | BoolDeclareAssign String Line 
		   | BoolDeclare String 
		   | ListDeclareAssign String Line 
		   | ListDeclare String
		   | FloatDeclareAssign String Line 
		   | FloatDeclare String  
           | ListsDeclareAssign String Line 
		   | ListsDeclare String 
           | VarAssign String Line
           | Print String
           | Append String Line | Pop String 
           | LinesSequencing Lines Lines
           deriving (Show,Eq)

data Line = Int Int | Var String |	TTrue | TFalse
		  | Negate Line
          | Plus Line Line 
          | Minus Line Line 
          | Times Line Line 
          | Divide Line Line 
          | Mod Line Line 
          | Div Line Line  
          
          | IntEquals Line Line 
          | GreaterEquals Line Line
          | LessEquals Line Line 
          | Greater Line Line 
          | Less Line Line 

          | Equals Line Line 
          | NotEquals Line Line 
          | Or Line Line
          | And Line Line 
          | Not Line

          | Len String
          | Get String Line
          | BreakLoop
          deriving (Show,Eq) 
} 