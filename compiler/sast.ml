(* Semantically checked AST *)

open Ast

type sexpr =
    SIntLiteral of int
  | SFloatLiteral of float
  | SStringLiteral of string
  | SBoolLiteral of bool
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SLocal of datatype * string * sexpr

type sfunc_decl = {
	sftype : datatype;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
  }

type sprogram = {
  functions : sfunc_decl list;
  stmts : sstmt list;
}
