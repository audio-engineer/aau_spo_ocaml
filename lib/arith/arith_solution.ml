type binop = Add | Multi | Div
type expr = Const of int | Binop of binop * expr * expr

exception Div_by_zero
exception Negative_const_int of int

let op_to_string op =
  match op with Add -> " + " | Multi -> " * " | Div -> " / "

let expr_string op s1 s2 = "( " ^ s1 ^ op_to_string op ^ s2 ^ " )"

let rec expr_to_string e =
  match e with
  | Const c -> string_of_int c
  | Binop (op, e1, e2) ->
      let s1 = expr_to_string e1 in
      let s2 = expr_to_string e2 in

      expr_string op s1 s2

(** Simplifies the expression. *)
let rec simplify e =
  match e with
  | Const _c -> e
  | Binop (op, Const 0, e) | Binop (op, e, Const 0) -> (
      match op with Add -> e | Multi | Div -> Const 0)
  | Binop (Multi, Const 1, e)
  | Binop (Multi, e, Const 1)
  | Binop (Div, e, Const 1) ->
      e
  | Binop (op, e1, e2) -> Binop (op, simplify e1, simplify e2)

let rec check_valid e =
  match e with
  | Const c -> if c < 0 then raise (Negative_const_int c) else Const c
  | Binop (Div, _e1, Const 0) -> raise Div_by_zero
  | Binop (op, e1, e2) -> Binop (op, check_valid e1, check_valid e2)

(** Prints the expression to the console. *)
let print e =
  try Printf.printf "%s\n" (expr_to_string (check_valid e)) with
  | Div_by_zero -> Printf.printf "Invalid input: the divisor is zero\n"
  | Negative_const_int c ->
      Printf.printf "Invalid input: the constant %d is a negative number\n" c

(** Interprets the arithmetic expression. *)
let rec interp e =
  match e with
  | Const c -> c
  | Binop (op, e1, e2) -> (
      let v1 = interp (simplify e1) in
      let v2 = interp (simplify e2) in

      match op with
      | Add -> v1 + v2
      | Multi -> v1 * v2
      | Div -> if v2 == 0 then raise Div_by_zero else v1 / v2)

let rec sum n =
  if n <= 0 then raise (Negative_const_int n)
  else
    let res = sum (n - 1) in
    Binop (Add, Const n, res)

let rec fact n =
  if n <= 0 then raise (Negative_const_int n)
  else
    let res = fact (n - 1) in
    Binop (Multi, Const n, res)
