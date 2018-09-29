%{

open Ast

%}

/* Déclaration des terminaux */
%token <int> Integer
%token <string> String
%token <string> ID
%token <float> Float

%token PPIPE COMMA DOT LPAR RPAR
%token NOT IS OR AND BETWEEN ASTERISK ON AS
%token SELECT FROM WHERE ALL DISTINCT LOWER UPPER SUBSTRING
%token FOR TRUE FALSE NULL FULL OUTER JOIN RIGHT LEFT INNER
%token NEQ LE GE PLUS MINUS EQ LT GT UMINUS
%token UNKNOWN SLASH TERM GROUPBY  CROSSJOIN


/* Précédences (priorité + associativité) des terminaux */
%left PLUS MINUS
%left ASTERISK DIV
%left IS
%left NOT
%left AND
%left OR
%left FROM
%left PPIPE
%left COMMA

%right UMINUS
%left NEQ LE GE EQ LT GT

%left FOR TRUE FALSE NULL FULL OUTER JOIN RIGHT LEFT INNER CROSSJOIN


/* Déclaration du non-terminal axiome (ici, ansyn) et du type de son attribut */
%type <Ast.query> ansyn
%start ansyn

%%

/* Déclaration de la grammaire avec les actions sémantiques */

ansyn:
  | TERM ansyn              { $2 }
  | simple_query TERM       { $1 }
;

colmn:
  | expression         { constColumn $1 }
  | colmn COMMA colmn  { concatListColumn $1 $3 }
  | expression AS ID   { constColumnId $1 $3 } 
;

projection:
  | ASTERISK { constAsterisk }
  | colmn    { constColumns $1 }
;

simple_query:
  | SELECT projection FROM source                           { constSelectAll $2 $4 constEmpty }
  | SELECT ALL projection FROM source                       { constSelectAll $3 $5 constEmpty }
  | SELECT DISTINCT projection FROM source                  { constSelectDist $3 $5 constEmpty }
  | SELECT projection FROM source WHERE condition           { constSelectAll $2 $4 $6 }
  | SELECT ALL projection FROM source WHERE condition       { constSelectAll $3 $5 $7 }
  | SELECT DISTINCT projection FROM source WHERE condition  { constSelectDist $3 $5 $7 }
;

expression:
  | attribute { constAttribute $1 }
  | LPAR expression RPAR { $2 }
  | Integer { constValueInteger $1 }
  | Float { constValueFloat $1 }
  | String {constValueString $1 }
  | expression PLUS expression { constCalcul $1 constPlus $3  }
  | expression MINUS expression { constCalcul $1 constMinus $3 }
  | expression DIV expression { constCalcul $1 constDivision $3 }
  | expression ASTERISK expression { constCalcul $1 constTimes $3 }
  | MINUS expression %prec UMINUS { constUMinus $2 }
  | expression PPIPE expression { constPipe $1 $3 }
  | UPPER LPAR expression RPAR { constUpper $3 }
  | LOWER LPAR expression RPAR { constLower $3 }
  | SUBSTRING LPAR expression FROM expression FOR expression RPAR { constSubString $3 $5 $7 }
;
attribute:
  | ID DOT ID { constTableColumn $1 $3 }
;

source:
  | ID { constId $1 }
  | LPAR simple_query RPAR { constQuery $2 }
  | source COMMA source { constComma $1 $3 }
  | source CROSSJOIN source { constCrossJoin $1 $3 }
  | source JOIN source ON condition { constJoinOp $1 constInnerJoin $3 $5}
  | source INNER JOIN  source ON condition { constJoinOp $1 constInnerJoin $4 $6}
  | source LEFT JOIN  source ON condition { constJoinOp $1 constLeftJoin $4 $6}
  | source LEFT OUTER JOIN source ON condition { constJoinOp $1 constLeftOuterJoin $5 $7}
  | source RIGHT JOIN source ON condition { constJoinOp $1 constRightJoin $4 $6}
  | source RIGHT OUTER JOIN source ON condition { constJoinOp $1 constRightOuterJoin $5 $7}
  | source FULL JOIN source ON condition { constJoinOp $1 constFullJoin $4 $6}
  | source FULL OUTER JOIN source ON condition { constJoinOp $1 constFullOuterJoin $5 $7}
;


condition:
  | predicate {constPredicate $1 }
  | NOT condition { constNegation $2 }
  | condition AND condition { constConjonction $1 $3 }
  | condition OR condition  { constDisjonction $1 $3 }
  | condition IS TRUE { constIs $1 constTrue }
  | condition IS FALSE { constIs $1 constFalse }
  | condition IS UNKNOWN { constIs $1 constUnknown }
  | condition IS NOT TRUE { constIsNot $1 constTrue }
  | condition IS NOT FALSE { constIsNot $1 constFalse }
  | condition IS NOT UNKNOWN { constIsNot $1 constUnknown }
;

predicate:
  | LPAR condition RPAR { constCond $2 }
  | expression EQ expression { constEvaluation $1 constEqual $3}
  | expression LT expression { constEvaluation $1 constLowerThan $3}
  | expression GT expression { constEvaluation $1 constGreaterThan $3}
  | expression NEQ expression { constEvaluation $1 constNotEqual $3}
  | expression LE expression { constEvaluation $1 constLowerEqual $3}
  | expression GE expression { constEvaluation $1 constGreaterEqual $3}
  | expression BETWEEN expression AND expression { constBetween $1 $3 $5 }
  | expression NOT BETWEEN expression AND expression { constBetweenNot $1 $4 $6 }
  | expression IS NULL { constIsNull $1 }
  | expression IS NOT NULL { constIsNotNull $1 }
;



/* source, query, condition,expression,... */
