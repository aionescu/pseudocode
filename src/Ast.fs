namespace Pseudoi.Ast

type Lit =
  | NumLit of float
  | BoolLit of bool
  | StringLit of string

type TypeName =
  | Int
  | Float
  | Bool
  | String

type LValue =
  | Ident of string
  | Subscript of string * Expr

and FuncCall =
  | FuncCall of string * Expr list

and Expr =
  | Lit of Lit
  | LValue of LValue
  | FuncCallExpr of FuncCall
  | NewArray of TypeName * Expr
  | UnOp of string * Expr
  | BinOp of Expr * string * Expr

type Statement =
  | FuncCallStatement of FuncCall
  | Assignment of LValue * Expr
  | Read of LValue * TypeName
  | Write of Expr list
  | If of Expr
  | ElseIf of Expr
  | Else
  | While of Expr
  | DoWhile of Expr
  | Until of Expr
  | Do
  | For of string * Expr * Expr * Expr option
  | Break
  | Continue
  | End
  | Empty

type Program = Program of Statement list