(* Abstract Syntax of While Language. *)

(* Parsed trees.
   This is the output of the parser and the input of the interpreter. *)

(* Identifiers. *)

(* Identifiers (variables) are normally represented by a value of type
   `string` but we wrap them into the following record type `ident`, using
   additional field `loc` that tracks the position (line & column) for each
   identifier. That is done to signal better syntax errors, e.g. when
   an unknown identifier is used in the program. *)
type location = Lexing.position * Lexing.position
type ident = { loc : location; id : string }

(* Unary operators. *)
type unop =
  | Uneg
  (* -e *)
  | Unot (* not e *)

(* Binary operators. *)
type binop =
  | Badd
  | Bsub
  | Bmul
  | Bdiv
  | Bmod (* + - * // % *)
  | Beq
  | Bneq
  | Blt
  | Ble
  | Bgt
  | Bge (* == != < <= > >= *)
  | Band
  | Bor (* and or *)

(* Constants. *)
type constant = Cbool of bool | Cstring of string | Cint of int

(* Expressions. *)
type expr =
  | Ecst of constant (* constant *)
  | Eunop of unop * expr (* unary operation *)
  | Ebinop of binop * expr * expr (* binary operation *)
  | Eident of ident (* variable *)

(* Statements. *)
type stmt =
  | Sif of expr * stmt * stmt (* conditional *)
  | Sassign of ident * expr (* modifying a variable *)
  | Sblock of stmt list (* a sequence of statements *)
  | Sprint of expr list (* printing a list of expressions *)
  | Swhile of expr * stmt (* while loop *)

(* a program is simply a statement. *)
type file = stmt
