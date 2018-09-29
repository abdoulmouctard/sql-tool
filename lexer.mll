{
open Parser
exception Eof
}


(* Déclaration du dictionnaire (regexp -> terminal/token) *)

rule anlex = parse
  | [' ' '\n' '\r' '\t']      { anlex lexbuf }
  | "--"                    { comment lexbuf }
  | "SELECT"                  { SELECT }
  | "FROM"                    { FROM }
  | "WHERE"                   { WHERE }
  | "ALL"                     { ALL }
  | "DISTINCT"                { DISTINCT }
  | "LOWER"                { LOWER }
  | "UPPER"                { UPPER }
  | "SUBSTRING"                { SUBSTRING }
  | "FOR"                   { FOR }
  | "NOT"                   { NOT }
  | "TRUE"                   { TRUE }
  | "FALSE"                   { FALSE }
  | "NULL"                   { NULL }
  | "IS"                   { IS }
  | "OR"                   { OR }
  | "AND"                   { AND }
  | "BETWEEN"                   { BETWEEN }
  | "FULL"                   { FULL }
  | "OUTER"                   { OUTER }
  | "JOIN"                   { JOIN }
  | "RIGHT"                   { RIGHT }
  | "LEFT"                   { LEFT }
  | "INNER"                   { INNER }
  | "UNKNOWN"                   { UNKNOWN }
  | "GROUP BY"                   { GROUPBY }
  | "CROSS JOIN"              { CROSSJOIN }
  | "AS"                      { AS }
  | "ON"                      { ON }
  | '*'                       { ASTERISK }
  | ','                       { COMMA }
  | '.'                       { DOT }
  | '('                       { LPAR }
  | ')'                       { RPAR }
  | '+'                       { PLUS }
  | '-'                       { MINUS }
  | '='                       { EQ }
  | '<'                       { LT }
  | '>'                       { GT }
  | '/'                       { SLASH }
  | ';'                       { TERM }
  | "<>"                      { NEQ }
  | "<="                      { LE }
  | ">="                      { GE }
  | "'"                        { (quote "" lexbuf) }
  | "\""                       { (qquote "" lexbuf) }
  | "||"                       { PPIPE }
  | (['0'-'9']+) '.' (['0'-'9']*) as lxm { Float(float_of_string lxm) }
  | ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9']* as lxm  { ID(lxm) }
  | ['0'-'9']+ as lxm         { Integer(int_of_string lxm) }
  | ['a'-'z''A'-'Z''0'-'9']* as lxm  { String(lxm) }
and quote str = parse
  | "'"              { String(str)  }
  | _ as lxm              { quote (str^( String.make 1 lxm)) lexbuf   }
and qquote str = parse
  | "\""              { ID(str)  }
  | _ as lxm              { qquote (str^(String.make 1 lxm) ) lexbuf }
and comment = parse
  | '\n' { anlex lexbuf }
  | _    { comment lexbuf }
