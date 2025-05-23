open Ast
open Format

(* Exception raised to signal a runtime error *)
exception Error of string

let error s = raise (Error s)

(* Values of Mini-Python.

   Two main differences wrt Python:

   - We use here machine integers (OCaml type `int`) while Python
     integers are arbitrary-precision integers (we could use an OCaml
     library for big integers, such as zarith, but we opt for simplicity
     here).

   - What Python calls a ``list'' is a resizeable array. In Mini-Python,
     there is no way to modify the length, so a mere OCaml array can be used.
*)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Print a value on standard output *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
      let n = Array.length a in
      printf "[";
      for i = 0 to n - 1 do
        print_value a.(i);
        if i < n - 1 then printf ", "
      done;
      printf "]"

(* Boolean interpretation of a value

   In Python, any value can be used as a Boolean: None, the integer 0,
   the empty string, and the empty list are all considered to be
   False, and any other value to be True.
*)
let is_false v =
  match v with
  | Vnone -> true
  | Vbool false -> true
  | Vint 0 -> true
  | Vstring "" -> true
  | Vlist arr when Array.length arr = 0 -> true
  | _ -> false (* TODO (question 2) *)

let is_true v = not (is_false v) (* TODO (question 2) *)

(* We only have global functions in Mini-Python *)
let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* The following exception is used to interpret `return` *)
exception Return of value

(* Local variables (function parameters and local variables introduced
   by assignments) are stored in a hash table that is passed to the
   following OCaml functions as parameter `ctx`. *)
(* type ctx = (string, value) Hashtbl.t *)

(* Interpreting an expression (returns a value) *)
let rec expr ctx = function
  | Ecst Cnone -> Vnone
  | Ecst (Cstring s) -> Vstring s
  (* arithmetic *)
  | Ecst (Cint n) -> Vint (Int64.to_int n) (* TODO (question 1) *)
  | Ebinop
      ( ((Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge)
         as op),
        e1,
        e2 ) -> (
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      match (op, v1, v2) with
      | Badd, Vint n1, Vint n2 -> Vint (n1 + n2) (* TODO (question 1) *)
      | Bsub, Vint n1, Vint n2 -> Vint (n1 - n2) (* TODO (question 1) *)
      | Bmul, Vint n1, Vint n2 -> Vint (n1 * n2) (* TODO (question 1) *)
      | Bdiv, Vint n1, Vint n2 ->
          if n2 = 0 then error "Cannot divide by zero"
          else Vint (n1 / n2) (* TODO (question 1) *)
      | Bmod, Vint n1, Vint n2 ->
          if n2 = 0 then error "Cannot divide by zero"
          else Vint (n1 mod n2) (* TODO (question 1) *)
      | Beq, _, _ -> Vbool (v1 = v2) (* TODO (question 2) *)
      | Bneq, _, _ -> Vbool (v1 <> v2) (* TODO (question 2) *)
      | Blt, _, _ -> Vbool (v1 < v2) (* TODO (question 2) *)
      | Ble, _, _ -> Vbool (v1 <= v2) (* TODO (question 2) *)
      | Bgt, _, _ -> Vbool (v1 > v2) (* TODO (question 2) *)
      | Bge, _, _ -> Vbool (v1 >= v2) (* TODO (question 2) *)
      | Badd, Vstring s1, Vstring s2 ->
          Vstring (s1 ^ s2) (* TODO (question 3) *)
      | Badd, Vlist l1, Vlist l2 ->
          Vlist (Array.append l1 l2) (* TODO (question 5) *)
      | _ -> error "unsupported operand types")
  | Eunop (Uneg, e1) -> (
      match expr ctx e1 with
      | Vint n -> Vint (-n)
      | _ ->
          error "Unsupported operand for unary negation" (* TODO (question 1) *)
      )
  (* Boolean *)
  | Ecst (Cbool b) -> Vbool b (* TODO (question 2) *)
  | Ebinop (Band, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_false v1 then v1 else expr ctx e2 (* TODO (question 2) *)
  | Ebinop (Bor, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_true v1 then v1 else expr ctx e2 (* TODO (question 2) *)
  | Eunop (Unot, e1) ->
      let v = expr ctx e1 in
      if is_false v then Vbool true else Vbool false (* TODO (question 2) *)
  | Eident { id; _ } ->
      if Hashtbl.mem ctx id then Hashtbl.find ctx id
      else raise (Failure ("Undefined variable: " ^ id)) (* TODO (question 3) *)
  (* function call *)
  | Ecall ({ id = "len"; _ }, [ e1 ]) -> (
      let v = expr ctx e1 in
      match v with
      | Vlist arr -> Vint (Array.length arr)
      | _ -> error "len expexts a list"
      (* TODO (question 5) *))
  | Ecall ({ id = "list"; _ }, [ Ecall ({ id = "range"; _ }, [ e1 ]) ]) -> (
      let v = expr ctx e1 in
      match v with
      | Vint n when n >= 0 ->
          let arr = Array.init n (fun i -> Vint i) in
          Vlist arr
      | Vint _ -> error "range(n) with negative n not handled"
      | _ -> error "list(range(n)) expects an integer n"
      (* TODO (question 5) *))
  | Ecall ({ id = f; _ }, el) -> (
      if not (Hashtbl.mem functions f) then error ("Undefined function: " ^ f)
      else
        let params, body = Hashtbl.find functions f in

        if List.length el <> List.length params then
          error ("Wrong number of arguments calling " ^ f)
        else
          let new_ctx = Hashtbl.create 16 in

          List.iter2
            (fun param_expr param_ident ->
              let arg_val = expr ctx param_expr in

              Hashtbl.replace new_ctx param_ident.id arg_val)
            el params;

          try
            stmt new_ctx body;

            Vnone
          with Return v -> v (* TODO (question 4) *))
  | Elist el ->
      let arr = Array.of_list (List.map (expr ctx) el) in
      Vlist arr (* TODO (question 5) *)
  | Eget (e1, e2) -> (
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      match (v1, v2) with
      | Vlist arr, Vint i ->
          if i < 0 || i >= Array.length arr then error "list index out of range"
          else arr.(i)
      | Vlist _, _ -> error "list index must be an integer"
      | _, _ -> error "attempting to index a non-list" (* TODO (question 5) *))

(* Interpreting a statement returns nothing but may raise exception `Return` *)
and stmt ctx = function
  | Seval e -> ignore (expr ctx e)
  | Sprint e ->
      print_value (expr ctx e);
      printf "@."
  | Sblock bl -> block ctx bl
  | Sif (e, s1, s2) ->
      let cond_value = expr ctx e in
      if is_true cond_value then stmt ctx s1
      else stmt ctx s2 (* TODO (question 2) *)
  | Sassign ({ id; _ }, e1) ->
      let v = expr ctx e1 in
      Hashtbl.replace ctx id v (* TODO (question 3) *)
  | Sreturn e ->
      let v = expr ctx e in
      raise (Return v)
      (* TODO (question 4) *)
  | Sfor ({ id; _ }, e, s) -> (
      let v_list = expr ctx e in
      match v_list with
      | Vlist arr ->
          Array.iter
            (fun elem ->
              Hashtbl.replace ctx id elem;
              stmt ctx s)
            arr
      | _ -> error "for loop expects a list"
      (* TODO (question 5) *))
  | Sset (e1, e2, e3) -> (
      let v_list = expr ctx e1 in
      let v_index = expr ctx e2 in
      let v_value = expr ctx e3 in
      match (v_list, v_index) with
      | Vlist arr, Vint i ->
          if i < 0 || i >= Array.length arr then
            error "list assignment index out of range"
          else arr.(i) <- v_value
      | Vlist _, _ -> error "list assignment index must be an integer"
      | _ ->
          error "attempting to assign into a non-list" (* TODO (question 5) *))

(* Interpreting a block (a sequence of statements) *)
and block ctx = function
  | [] -> ()
  | s :: sl ->
      stmt ctx s;
      block ctx sl

(* Interpreting a file
   - `dl` is a list of function definitions (see type `def` in ast.ml)
   - `s` is a statement (the toplevel code)
*)
let file (dl, s) =
  (* TODO (question 4) *)
  List.iter
    (fun (f_name, params, body) ->
      Hashtbl.replace functions f_name.id (params, body))
    dl;

  stmt (Hashtbl.create 16) s
