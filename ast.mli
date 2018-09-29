open ModuleR
open Env
open Value
type attribute
type operateur
type condOperateur
type joinOp
type expression
type column
type projection
type atom
type condition
type predicate
type query
type source

val string_of_query: query -> string
val eval_query: (R.relation * (R.attribute env)) env -> query -> R.relation
val attributeFromColumns: column list -> (R.relation * (R.attribute env)) env -> R.relation -> (domain * (R.tuple -> value option)) list

(* type atom *)
val constTrue : atom
val constFalse : atom
val constUnknown : atom

(* type query *)
val constSelectAll : projection -> source -> condition -> query
val constSelectDist : projection -> source -> condition -> query


val constAsterisk: projection
val constColumns : column list -> projection

(* type condition *)
val constEmpty : condition
val constPredicate : predicate -> condition
val constNegation : condition -> condition
val constConjonction : condition -> condition -> condition
val constDisjonction : condition -> condition -> condition
val constIs : condition -> atom -> condition
val constIsNot : condition -> atom -> condition

(* type predicate *)
val constCond: condition -> predicate
val constEvaluation : expression -> condOperateur -> expression -> predicate
val constBetween : expression -> expression -> expression -> predicate
val constBetweenNot : expression -> expression -> expression -> predicate
val constIsNull : expression -> predicate
val constIsNotNull : expression -> predicate

(* type condOperateur *)
val constEqual : condOperateur
val constNotEqual : condOperateur
val constLowerThan : condOperateur
val constGreaterThan : condOperateur
val constLowerEqual : condOperateur
val constGreaterEqual : condOperateur

(* type source *)
val constId : string -> source
val constQuery : query -> source
val constComma : source -> source -> source
val constCrossJoin : source -> source -> source
val constJoinOp : source -> joinOp -> source -> condition -> source

(* type joinOp *)
val constInnerJoin : joinOp
val constLeftJoin : joinOp
val constRightJoin : joinOp
val constFullJoin : joinOp
val constLeftOuterJoin : joinOp
val constRightOuterJoin : joinOp
val constFullOuterJoin : joinOp

(* type column *)
val constColumn: expression -> column list
val constColumnId: expression -> string -> column list


(* type expression *)
val constValueInteger : int -> expression
val constValueFloat : float -> expression
val constValueString : string -> expression

val constAttribute : attribute -> expression
val constCalcul : expression -> operateur -> expression -> expression
val constUpper : expression -> expression
val constUMinus : expression -> expression
val constLower: expression -> expression
val constPipe : expression -> expression -> expression
val constSubString : expression -> expression -> expression -> expression

(* type operateur *)
val constPlus : operateur
val constMinus: operateur
val constTimes : operateur
val constDivision : operateur

(* type attribute *)
val constTableColumn : string -> string -> attribute


val concatListColumn: column list -> column list -> column list
