(** Abstract Syntax Trees obtained from the parsing phase *)

type loc = Lexing.position * Lexing.position

type ident = { id: string; id_loc: loc }

type typ =
  | Tint
  | Tstructp of ident

type unop =
  | Unot | Uminus

type binop =
  | Beq | Bneq | Blt | Ble | Bgt | Bge | Badd | Bsub | Bmul | Bdiv
  | Band | Bor

(** Expression C *)
type expr =
  { expr_node: expr_node;
    expr_loc : loc }

and expr_node =
  | Econst of int32
  | Eident of ident
  | Earrow of expr * ident
  | Eassign_local of ident * expr
  | Eassign_field of expr * ident * expr
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Ecall of ident * expr list
  | Esizeof of ident
  | Emalloc of ident

type decl_var = typ * ident

(** Statement *)
type stmt =
  { stmt_node: stmt_node;
    stmt_loc : loc }

and stmt_node =
  | Sskip
  | Sexpr of expr
  | Sif of expr * stmt * stmt
  | Swhile of expr * stmt
  | Sblock of block
  | Sreturn of expr

and block =
  decl_var list * stmt list

type decl_struct = ident * decl_var list

type decl_fun = {
  fun_typ : typ;
  fun_name : ident;
  fun_formals : decl_var list;
  fun_body : block
}

(** A mini-C file is a list of declarations of structurues
    followed by a list of declarations of functions *)
type decl =
  | Dstruct of decl_struct
  | Dfun of decl_fun

type file = decl list
