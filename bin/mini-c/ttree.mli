(* Typed Abstract Syntax Trees *)

(* identifiers *)
type ident = string

(* Types, structures, and fields *)
type typ =
  | Tint                        (* integer *)
  | Tstructp of structure       (* struct *)
  | Tvoidstar                   (* void* *)

and structure = {
  str_name : ident;                   (* structure's name *)
  str_size : int;                             (* in words *)
  str_fields : (ident, field) Hashtbl.t;
  (* hash table: field_name --> (field_typ, field_offset) *)
}

and field = {
  field_typ : typ;
  field_ofs : int;
}

(* Unary operations *)
type unop = Ptree.unop

(* Binary operations *)
type binop = Ptree.binop

(* Typed variables *)
type decl_var = typ * ident

(* Typed Expressions *)
type expr =
  { expr_node : expr_node;
    expr_typ  : typ }

and expr_node =
  | Econst of int32                    (* int constants *)
  | Eunop of unop * expr               (* unary operations *)
  | Ebinop of binop * expr * expr      (* binary operations *)
  | Eaccess_local of ident             (* x *)
  | Eassign_local of ident * expr      (* x = e *)
  | Eaccess_field of expr * int        (* e.f_i, where f_i is offset *)
  | Eassign_field of expr * int * expr (* e.f_i -> e *)
  | Ecall of ident * expr list         (* function call *)
  | Esizeof of structure               (* sizeof *)
  | Emalloc of structure               (* malloc *)

type stmt =
  | Sskip
  | Sexpr of expr
  | Sif of expr * stmt * stmt
  | Swhile of expr * stmt
  | Sblock of block
  | Sreturn of expr

and block =
  decl_var list * stmt list

and decl_fun = {
  fun_typ : typ;
  fun_name : ident;
  fun_formals : decl_var list;
  fun_body : block
}

type file = {
  funs : decl_fun list;
}
