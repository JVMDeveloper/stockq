(* Abstract Syntax Tree for StockQ *)

type op = Add | Sub | Mult | Div | Mod | Equal |
          Addeq | Subeq | Multeq | Diveq | Modeq |
          Neq | Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not

type primitive = Int | Float | Bool | Void | Stock | Order |
                 Portfolio | String | Array | Struct

type datatype =
  | Primitive of primitive
  | Arraytype of primitive * int

type bind = datatype * string

type expr =
    IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
  | BoolLiteral of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | ArrayAssign of expr * expr
  | Call of string * expr list
  | ArrayCreate of datatype * expr list
  | ArrayAccess of expr * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Local of datatype * string * expr

type func_decl = {
	ftype : datatype;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = func_decl list * stmt list
