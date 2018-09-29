open ModuleR
open Env
open Value
(*  *)

type attribute =
  | TableColumn of string*string

type operateur =
  | Plus
  | Minus
  | Times
  | Division

type condOperateur =
  | Equal
  | NotEqual
  | LowerThan
  | GreaterThan
  | LowerEqual
  | GreaterEqual

type joinOp =
  | InnerJoin
  | LeftJoin
  | RightJoin
  | FullJoin
  | LeftOuterJoin
  | RightOuterJoin
  | FullOuterJoin


type expression =
  | Attribute   of attribute
  | Value       of value
  | Calcul      of expression * operateur * expression
  | Upper       of expression
  | Lower       of expression
  | UMinus      of expression
  | Pipe        of expression * expression
  | SubString   of expression * expression * expression

type column =
  | Column    of expression
  | ColumnId  of expression * string

type projection =
  | Asterisk
  | Columns   of column list

type atom =
  | True
  | False
  | Unknown


type condition =
  | Empty
  | Predicate    of predicate
  | Negation    of condition
  | Conjonction of condition * condition
  | Disjonction of condition * condition
  | Is       of condition * atom
  | IsNot        of condition * atom
and predicate =
  | Cond        of condition
  | Evaluation  of expression * condOperateur * expression
  | Between     of expression * expression * expression
  | BetweenNot  of expression * expression * expression
  | IsNull      of expression
  | IsNotNull   of expression



type source =
  | Id          of string
  | Query       of query
  | Comma       of source * source
  | CrossJoin   of source * source
  | JoinOp      of source * joinOp * source * condition
and query =
  | SelectAll   of projection * source * (condition ) (* sans option *)
  | SelectDist  of projection * source * (condition )

(* type atom *)
let constTrue = True;;
let constFalse = False;;
let constUnknown = Unknown;;

(* type query *)
let constSelectAll  proj src cond = SelectAll(proj,src,cond);;
let constSelectDist proj src cond =
  match proj with
  | Asterisk  -> failwith "Le selecteur * n'est pas autorisé avec DISTINCT "
  | _ -> SelectDist(proj,src,cond);;

(* type column  *)
let constColumn expr = [Column(expr)];;
let constColumnId expr str = [ColumnId(expr,str)];;

(* type projection  *)
let constAsterisk = Asterisk;;
let constColumns list = Columns(list);;

(* type condition *)
let constEmpty = Empty;;
let constPredicate pred = Predicate(pred);;
let constNegation cond = Negation(cond);;
let constConjonction c1 c2 = Conjonction(c1,c2);;
let constDisjonction c1 c2 = Disjonction(c1,c2);;
let constIs cond atom = Is(cond,atom);;
let constIsNot cond atom = IsNot(cond,atom);;

(* type predicate *)
let constCond cond = Cond(cond);;
let constEvaluation expr1 op expr2 = Evaluation(expr1,op,expr2);;
let constBetween expr1 expr2 expr3 = Between(expr1,expr2,expr3);;
let constBetweenNot expr1 expr2 expr3 = BetweenNot(expr1,expr2,expr3);;
let constIsNull expr = IsNull(expr);;
let constIsNotNull expr = IsNotNull(expr);;

(* type condOperateur *)
let constEqual = Equal;;
let constNotEqual = NotEqual;;
let constLowerThan = LowerThan;;
let constGreaterThan = GreaterThan;;
let constLowerEqual = LowerEqual;;
let constGreaterEqual = GreaterEqual;;

(* type source *)
let constId id = Id(id);;
let constQuery query = Query(query);;
let constComma src1 src2 = Comma(src1,src2);;
let constCrossJoin src1 src2 = CrossJoin(src1,src2);;
let constJoinOp src1 op src2 cond = JoinOp(src1,op,src2,cond);;

(* type joinOp *)
let constInnerJoin = InnerJoin;;
let constLeftJoin = LeftJoin;;
let constRightJoin = RightJoin;;
let constFullJoin = FullJoin;;
let constLeftOuterJoin = LeftOuterJoin;;
let constRightOuterJoin = RightOuterJoin;;
let constFullOuterJoin = FullOuterJoin;;

(* type expression *)
let constValueInteger const = Value(VInt const);;
let constValueFloat const = Value(VFloat const );;
let constValueString const = Value(VVChar const);;

let constAttribute attr = Attribute(attr);;
let constCalcul expr1 op expr2= Calcul(expr1,op,expr2);;
let constUpper expr = Upper(expr);;
let constUMinus expr = UMinus(expr);;
let constLower expr = Lower(expr);;
let constPipe expr1 expr2 = Pipe(expr1,expr2);;
let constSubString expr1 expr2 expr3 = SubString(expr1,expr2,expr3);;

(* type operateur *)
let constPlus = Plus;;
let constMinus = Minus;;
let constTimes = Times;;
let constDivision = Division;;

(* type attribute *)
let constTableColumn str1 str2 = TableColumn(str1,str2);;

(* let constPipe =Pipe;; *)


let concatListColumn l1 l2 = l1 @ l2;;

let string_of_joinOp op=
  match op with
  | InnerJoin   ->  "INNER JOIN"
  | LeftJoin    ->  "LEFT JOIN"
  | RightJoin   ->  "RIGHT JOIN"
  | FullJoin    ->  " FULL JOIN"
  | LeftOuterJoin   ->  "LEFT OUTER JOIN"
  | RightOuterJoin    ->  "RIGHT OUTER JOIN"
  | FullOuterJoin   ->  "FULL OUTER JOIN"


let string_of_atom atom=
  match atom with
  | True -> "TRUE"
  | False-> "FALSE"
  | Unknown -> "UNKNOWN"

let string_of_condOp cond=
  match cond with
  | Equal -> "="
  | NotEqual-> "<>"
  | LowerThan -> "<"
  | GreaterThan-> ">"
  | LowerEqual-> "<="
  | GreaterEqual-> ">="

let string_of_operateur op=
  match op with
  | Plus-> "+"
  | Minus-> "-"
  | Times-> "*"
  | Division-> "/"


let rec string_of_query query =
  match query with
  | SelectAll(p,s,c)  ->
    ( Printf.sprintf "SELECT %s FROM %s WHERE %s"
      (string_of_projection p) (string_of_source s) (string_of_condition c) )
  | SelectDist(p,s,c)  ->
    ( Printf.sprintf "SELECT DISTINCT %s FROM %s WHERE %s"
      (string_of_projection p) (string_of_source s) (string_of_condition c) )
and string_of_source source =
  match source with
  | Id(id)       -> Printf.sprintf "%s" id
  | Query(q)     -> Printf.sprintf "(%s)" (string_of_query q)
  | Comma(s1,s2) -> Printf.sprintf "%s,%s" (string_of_source s1) (string_of_source s2)
  | CrossJoin(s1,s2) -> Printf.sprintf "CROSS JOIN %s %s" (string_of_source s1) (string_of_source s2)
  | JoinOp(s1,op,s2,c) ->
    (Printf.sprintf " %s %s %s ON %s " (string_of_source s1)
        (string_of_joinOp op) (string_of_source s2) (string_of_condition c) )
and string_of_projection proj =
  match proj with
  | Asterisk     -> "*"
  | Columns(c_list) -> string_of_columns c_list
and string_of_columns c_list =
  match c_list with
  | []   -> ""
  | t::q -> ( match q with
              | [] -> (match t with Column(e) | ColumnId(e,_) -> string_of_expression e)
              | _ -> Printf.sprintf "%s, %s" (match t with
                        | Column(e)   -> Printf.sprintf "%s" (string_of_expression e)
                        | ColumnId(e,s) -> Printf.sprintf "%s AS %s " (string_of_expression e) s) (string_of_columns q)
            )
and string_of_condition cond =
  match cond with
  | Empty         -> "TRUE"
  | Predicate(p)  -> string_of_predicate p
  | Negation(cond)-> string_of_condition cond
  | Conjonction(cond1,cond2) -> Printf.sprintf "(%s) AND (%s)" (string_of_condition cond1) (string_of_condition cond2)
  | Disjonction (cond1,cond2) -> Printf.sprintf "(%s) OR (%s)" (string_of_condition cond1) (string_of_condition cond2)
  | Is (cond,atom)  ->    Printf.sprintf "%s IS %s" (string_of_condition cond) (string_of_atom atom)
  | IsNot(cond,atom)  ->    Printf.sprintf "%s IS NOT %s" (string_of_condition cond) (string_of_atom atom)
and string_of_predicate pred =
  match pred with
  | Cond(cond)    -> string_of_condition cond
  | Evaluation(expr1,c_op,expr2)  -> Printf.sprintf "(%s) %s (%s)" (string_of_expression expr1) (string_of_condOp c_op) (string_of_expression expr2)
  | Between (expr1,expr2,expr3) ->Printf.sprintf "(%s) BETWEEN (%s) AND (%s)" (string_of_expression expr1) (string_of_expression expr2) (string_of_expression expr3)
  | BetweenNot (expr1,expr2,expr3) ->Printf.sprintf "(%s) NOT BETWEEN (%s) AND (%s)" (string_of_expression expr1) (string_of_expression expr2) (string_of_expression expr3)
  | IsNull (expr) -> Printf.sprintf "(%s) IS NULL" (string_of_expression expr)
  | IsNotNull (expr) -> Printf.sprintf "(%s) IS NOT NULL" (string_of_expression expr)
and string_of_expression expr=
  match expr with
  | Attribute(TableColumn (str1, str2)) -> Printf.sprintf "%s.%s" str1 str2
  | Value(const)     -> Printf.sprintf "%s" (string_of_value const )
  | Calcul(expr1,op,expr2) -> Printf.sprintf "(%s) %s (%s)" (string_of_expression expr1) (string_of_operateur op) (string_of_expression expr2)
  | Upper (expr) -> Printf.sprintf " UPPER (%s)" (string_of_expression expr)
  | Lower (expr) -> Printf.sprintf " LOWER (%s)" (string_of_expression expr)
  | UMinus (expr) -> Printf.sprintf " -(%s)" (string_of_expression expr)
  | Pipe  (expr1,expr2) -> Printf.sprintf "(%s) || (%s)" (string_of_expression expr1) (string_of_expression expr2)
  | SubString  (expr1,expr2,expr3) -> Printf.sprintf "SUBSTRING((%s) FROM (%s) FOR (%s))" (string_of_expression expr1)  (string_of_expression expr2) (string_of_expression expr3)

(*  *)
let eval_atom atom =
  match atom with
  | True -> true
  | False | Unknown -> false

let fonctOperateurInteger op =
  match op with
  | Plus     -> ( + )
  | Minus    -> ( - )
  | Times    -> ( * )
  | Division -> ( / )

let fonctOperateurFloat op =
  match op with
  | Plus     -> ( +. )
  | Minus    -> ( -. )
  | Times    -> ( *. )
  | Division -> ( /. )

(*
let calcul_expression c1 c2 op =
  match c1,c2 with
  | Integer(i1),Integer(i2) -> Integer((fonctOperateurInteger op) i1 i2)
  | Float(f1),Float(f2) -> Float((fonctOperateurFloat op) f1 f2)
  | Float(f1),Integer(i) -> Float((fonctOperateurFloat op) f1  (float_of_int i))
  | Integer(i),Float(f) -> Float((fonctOperateurFloat op) (float_of_int i) f)
  | _,_ -> failwith "operation non permise !"*)

let attributeFromTableColumn expr env =
  match expr with
  |  Attribute(TableColumn(s1,s2))-> (
     match (find s1 env) with
          | Some(_,a)->(
            match (find s2 a) with
            | Some(a1) -> a1
            |  _ -> failwith ( Printf.sprintf "L'attribu %s n'existe pas dans la table %s " s2 s1)
            )
          |None -> failwith ( Printf.sprintf "La table %s n'existe pas la base de données " s1)
  )
  | _ -> failwith "rrrrr"

let conditionToAttribute cond env =
  match cond with
  | Predicate(Evaluation(e1,c,e2))->(
    match c with
    | Equal ->((attributeFromTableColumn e1 env),(attributeFromTableColumn e2 env))
    | _ -> failwith "eee"
  )
  |  _ -> failwith "eee"


let evaluator cop =
  match cop with
  | Equal        -> equal
  | NotEqual     -> notequal
  | LowerThan    -> lowerequal
  | GreaterThan  -> greaterthan
  | LowerEqual   -> lowerequal
  | GreaterEqual -> greaterequal


(*  ************************************************************************ *)


let rec eval_query r_env query =
  match query with
  | SelectAll(proj,src,cond)  ->(
    R.selection (fun t->eval_condition cond t r_env) (eval_projection proj r_env (eval_source src r_env) ) )
  | SelectDist(proj,src,cond) ->
    ( R.distinct (eval_projection proj r_env (eval_source src r_env) ) )
and eval_source src env =
  match src with
  | Id(id) -> (
      match (find id env ) with
        | Some((a,_))  -> a
        | _ -> failwith (Printf.sprintf "La table %s n'existe pas !" id)
    )
  | JoinOp(s1,jop,s2,cond) -> (
    let a1,a2 = (conditionToAttribute cond env) in
      match jop with
      | InnerJoin     -> R.innerjoin (fun t1 t2 -> (R.attribute a1 t1) =  (R.attribute a2 t2)) (eval_source s1 env) (eval_source s2 env)
      | LeftJoin      -> R.inter (eval_source s1 env) (R.innerjoin (fun t1 t2 -> (R.attribute a1 t1) =  (R.attribute a2 t2)) (eval_source s1 env) (eval_source s2 env) )
      | RightJoin     -> R.innerjoin (fun t1 t2 -> (R.attribute a1 t1) =  (R.attribute a2 t2)) (eval_source s1 env) (eval_source s2 env)
      | FullJoin      -> R.innerjoin (fun t1 t2 -> (R.attribute a1 t1) =  (R.attribute a2 t2)) (eval_source s1 env) (eval_source s2 env)
      | LeftOuterJoin -> R.leftouterjoin (fun t1 t2 -> (R.attribute a1 t1) =  (R.attribute a2 t2)) (eval_source s1 env) (eval_source s2 env)
      | RightOuterJoin-> R.innerjoin (fun t1 t2 -> (R.attribute a1 t1) =  (R.attribute a2 t2)) (eval_source s1 env) (eval_source s2 env)
      | FullOuterJoin -> R.fullouterjoin (fun t1 t2 -> (R.attribute a1 t1) =  (R.attribute a2 t2)) (eval_source s1 env) (eval_source s2 env)
    )
  | Comma(s1,s2) -> R.crossjoin (eval_source s1 env) (eval_source s2 env)
  | CrossJoin(s1,s2)-> R.crossjoin (eval_source s1 env) (eval_source s2 env)
  | _ -> failwith "n'existe pas"
and eval_projection proj env relation =
  match proj with
  | Asterisk -> relation
  | Columns(l) -> (
      match l with
        | []   -> failwith "liste de colonnes est vide !"
        | _ -> R.projection (attributeFromColumns l env relation) relation
  )
and eval_expression expr env t  =
  match expr with
  | Attribute(TableColumn(s1,s2)) ->(
      match (find s1 env) with
      | Some((a,b)) -> (
          match find s2 b with
          | Some(c) -> match R.attribute c t with |Some(v)->v |None->NULL
          | None -> failwith (Printf.sprintf "\"%s\" n'est pas une colonne dans la table !" s2)
        )
      | None -> failwith (Printf.sprintf "La table \"%s\" n'existe pas !" s1)
    )
  | Value(const) -> const
  | Calcul(e1,op,e2) -> (
      match op with
      | Plus    -> (add (eval_expression e1 env t) (eval_expression e2 env t))
      | Minus   -> (minus (eval_expression e1 env t) ( eval_expression e2 env t))
      | Times   -> (mul (eval_expression e1 env t) (eval_expression e2 env t))
      | Division-> (div (eval_expression e1 env t) (eval_expression e2 env t))
    )
  | Upper(expr)   -> (
      match (eval_expression expr env t) with
      | VVChar(s) -> VVChar(String.uppercase s)
      |_ -> failwith (Printf.sprintf "Operateur UPPER non defini pour %s" (string_of_expression expr) )
    )
  | Lower(expr)   ->(
      match (eval_expression expr env t) with
      | VVChar(s) -> VVChar(String.lowercase s)
      |_ -> failwith (Printf.sprintf "Operateur LOWER non defini pour %s" (string_of_expression expr) )
    )
  | UMinus(expr)  -> (
      match (eval_expression expr env t) with
      | VInt(i) -> VInt(- i)
      | VFloat(f)   -> VFloat(-. f)
      |_ -> failwith (Printf.sprintf "Operateur - non defini pour %s" (string_of_expression expr) )
    )
  | Pipe(e1,e2) ->concat (eval_expression e1 env t) (eval_expression e2 env t)

  | SubString(e1,e2,e3) -> (
      match (eval_expression e1 env t),(eval_expression e2 env t),(eval_expression e3 env t) with
      | VVChar(s1),VInt(i1),VInt(i2) -> VVChar (String.sub s1 i1 i2)
      | _,_,_ -> failwith (Printf.sprintf "Operateur SubString n'est pas applicable entre %s,%s et %s "
                      (string_of_expression e1) (string_of_expression e2) (string_of_expression e3) )
  )
and eval_condition cond tuple env =
  match cond with
  | Empty       -> true
  | Predicate(p) -> (eval_predicate p tuple env)
  | Negation(c)   -> (not (eval_condition c tuple env))
  | Conjonction(c1,c2)   -> ((eval_condition c1 tuple env) && (eval_condition c2 tuple env))
  | Disjonction(c1,c2)   -> ((eval_condition c1 tuple env) || (eval_condition c2 tuple env))
  | Is(c1,at) -> ((eval_condition c1 tuple env) == (eval_atom at))
  | IsNot(c1,at) -> ( (eval_condition c1 tuple env) <> (eval_atom at))
and eval_predicate pred tuple env =
  match pred  with
  | Cond(cond) -> (eval_condition cond tuple env)
  | Evaluation(e1,c,e2) ->( (evaluator c) (eval_expression e1 env tuple) (eval_expression e2 env tuple))
  | Between(e1,e2,e3)   -> (
      ((evaluator LowerEqual ) (eval_expression e2 env tuple) (eval_expression e1 env tuple)) &&
      ((evaluator LowerEqual ) (eval_expression e1 env tuple) (eval_expression e3 env tuple))
    )
  | BetweenNot(e1,e2,e3)-> (
      not ((evaluator LowerEqual ) (eval_expression e2 env tuple) (eval_expression e1 env tuple)) ||
      not ((evaluator LowerEqual ) (eval_expression e1 env tuple) (eval_expression e3 env tuple))
    )
  | IsNull(e) -> (
    match eval_expression e env tuple with
    | VVChar a -> ((String.length a) = 0)
    | NULL-> true
    | _ -> false
 )


  | IsNotNull(e)  -> (
    match eval_expression e env tuple with
    | VVChar a -> not ((String.length a) = 0)
    | NULL-> false
    | _ -> true
 )

  and attributeFromColumns l env rel =
  let rec loop l ret =
    match l with
    | [] -> ret
    | t::q -> (
        (match t with
        | Column(e) ->(
                match e with
                | Attribute((TableColumn(a1,a2))) -> (
                    ( match (find a1 env) with
                      | Some((a,b)) -> (
                          match find a2 b with
                          | Some(c) -> ((R.domain a c),(R.attribute c))
                          | None -> failwith (Printf.sprintf "\"%s\" n'est pas une colonne dans la table !" a2)
                        )
                      | None -> failwith (Printf.sprintf "La table \"%s\" n'existe pas !" a1) )
                  )
                | _ -> (DVChar,(fun t ->Some(eval_expression e env t)))
          )
        | ColumnId(e,id) ->(
                match e with
                | Attribute((TableColumn(a1,a2))) -> (
                    ( match (find a1 env) with
                      | Some((a,b)) ->  (
                          match find a2 b with
                          | Some(c) -> let en=Env.add id c b in ((R.domain a c),(fun t ->Some(eval_expression e env t)))
                          | None -> failwith (Printf.sprintf "\"%s\" n'est pas une colonne dans la table !" a2)
                        )
                      | None -> failwith (Printf.sprintf "La table \"%s\" n'existe pas !" a1) )
                  )
                | _ -> (DVChar,(fun t ->Some(eval_expression e env t)))
          )


      )
      )::(loop q ret)
  in loop l [];;




(*  *)
