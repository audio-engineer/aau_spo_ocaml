(* Interpreter for While Language. *)
open Ast
open Format

(* ************************************************************************** *)
(*                                   Utilities                                *)
(* ************************************************************************** *)

(* Exception raised to signal a runtime error *)
exception Error of string

let error s = raise (Error s)

(* Values. *)
type value = Vbool of bool | Vint of int | Vstring of string

(* Variables are stored in a hash table that is passed to the
   following OCaml functions as parameter `ctx`. *)
type ctx = (string, value) Hashtbl.t

(* Print a value on standard output *)
let print_value = function
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s

(* Printing a list of values. *)
let rec print_values vl =
  match vl with
  | [] -> printf "@."
  | [ v ] ->
      print_value v;
      printf "@."
  | v :: vtl ->
      print_value v;
      print_string " ";
      print_values vtl

(* ************************************************************************** *)
(*                          Interpreting expressions                          *)
(* ************************************************************************** *)

(* Interpreting expressions. *)
let rec interp_expr ctx = function
  | Ecst c -> interp_const c
  | Eunop (op, e1) -> interp_unop ctx op e1
  | Ebinop (op, e1, e2) -> interp_binop ctx op e1 e2
  | Eident { id; _ } -> ( try Hashtbl.find ctx id with _ -> error "not found")

(* Interpreting constants. *)
and interp_const = function
  | Cbool b -> Vbool b
  | Cstring s -> Vstring s
  | Cint n -> Vint n

(* Interpreting unary operations. *)
and interp_unop ctx op e1 =
  let v1 = interp_expr ctx e1 in
  match op with
  | Uneg -> (
      let v1 = interp_expr ctx e1 in
      match v1 with
      | Vint n1 -> Vint (-n1)
      | _ -> error "wring unary operand type: argument must be of integer type!"
      )
  | Unot -> (
      match v1 with
      | Vbool b1 -> Vbool (not b1)
      | _ -> error "wring unary operand type: argument must be of Boolean type!"
      )

(* Interpreting binary operations. *)
and interp_binop ctx op e1 e2 =
  match op with
  | Badd | Bsub | Bmul | Bdiv | Bmod -> interp_binop_arith ctx op e1 e2
  | _ (* all other cases *) -> interp_binop_bool ctx op e1 e2

(* Interpreting binary arithmetic operations return a numerical value. *)
(* We assume that op can only be a binary operation evaluating
   to a integer value, as it is called from the `interp_binop`. *)
and interp_binop_arith ctx op e1 e2 =
  let v1 = interp_expr ctx e1 in
  let v2 = interp_expr ctx e2 in
  match (v1, v2) with
  | Vint n1, Vint n2 -> (
      match op with
      | Badd -> Vint (n1 + n2)
      | Bsub -> Vint (n1 - n2)
      | Bmul -> Vint (n1 * n2)
      | Bmod -> Vint (n1 mod n2)
      | Bdiv -> if n2 = 0 then error "division by zero!" else Vint (n1 / n2)
      | _ -> assert false (* other operations excluded by asssumption. *))
  | _ -> error "wring operand type: arguments must be of integer type!"

(* Interpreting binary operations returning a Boolean value. *)
(* We assume that op can only be a binary operation evaluating
   to a Boolean value, as it is called from the `interp_binop`. *)
and interp_binop_bool ctx op e1 e2 =
  (* We first treat cases where `op` is a logical operation `Band` or `Bor`
     separately for efficiency. *)
  if op = Band then
    match interp_expr ctx e1 with
    | Vbool b1 ->
        if b1 then
          match interp_expr ctx e2 with
          | Vbool b2 -> Vbool b2
          | _ -> error "unsupported operand types"
        else Vbool false
    | _ -> error "unsupported operand types"
  else if op = Bor then
    match interp_expr ctx e1 with
    | Vbool b1 -> (
        if b1 then Vbool true
        else
          match interp_expr ctx e1 with
          | Vbool b2 -> Vbool b2
          | _ -> error "unsupported operand types")
    | _ -> error "unsupported operand types"
  else
    (* In all other binary comparision operations, we can evaluate both
       arguments first: *)
    let v1 = interp_expr ctx e1 in
    let v2 = interp_expr ctx e2 in
    match op with
    | Beq -> Vbool (v1 = v2)
    | Bneq -> Vbool (not (v1 = v2))
    | Blt -> Vbool (v1 < v2)
    | Ble -> Vbool (v1 <= v2)
    | Bgt -> Vbool (v1 > v2)
    | Bge -> Vbool (v1 >= v2)
    | _ -> assert false (* other operations excluded by asssumption. *)

(* ************************************************************************** *)
(*                          Interpreting statements                           *)
(* ************************************************************************** *)

(* Interpreting a statement *)
let rec stmt ctx = function
  | Sif (e, s1, s2) -> (
      match interp_expr ctx e with
      | Vbool b1 -> if b1 then stmt ctx s1 else stmt ctx s2
      | _ -> error "wrong type : bool expected")
  | Sassign ({ id; _ }, e1) -> Hashtbl.replace ctx id (interp_expr ctx e1)
  | Sblock bl -> block ctx bl
  | Sprint el -> print_values (List.map (fun e -> interp_expr ctx e) el)
  | Swhile (e, s) -> (
      match interp_expr ctx e with
      | Vbool b ->
          if b then (
            stmt ctx s;
            stmt ctx (Swhile (e, s)))
          else printf ""
      | _ -> error "wrong type : bool expected")

and block ctx = function
  | [] -> ()
  | s :: sl ->
      stmt ctx s;
      block ctx sl

(* ************************************************************************** *)
(*                          Interpreting the program                          *)
(* ************************************************************************** *)

let file s = stmt (Hashtbl.create 16) s
