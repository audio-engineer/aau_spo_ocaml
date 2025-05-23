(* ---------------------------- First program ------------------------------ *)

print_string "hello world!\n"

(* We can compile it with $ ocamlopt -o hello hello.ml
and execute it:

 $ ./hello hello world!  *)

(* -------------------------------------------------------------------------- *)
(* 1. Declarations *)

(* An OCaml program is made up of any sequence of expressions to be evaluated
   and declarations. A declaration affects the result of evaluating an
   expression to a variable, and is introduced by the let keyword.
   For example: *)

let x = 1 + 2
let () = print_int x
let y = x * x
let () = print_int y;;

(* For the variables x,y above
   - they are always initialised
   - their type can be inferred
   - their content is immutable.

we will see later how to define and use mutable data structures. *)

(* -------------------- Expressions and Instructions ------------------------ *)

(*
In functional programming, there is no distinction between expressions
and statements. In imperative languages, these are two separate
syntactic categories. For example, in Java, it is not possible
to have an expression like:

1 + (if (x == 0) f(); else g();)

or a statement like:

2 * { int s = 0; for (int i = 0; i < 10; i++) s += i; return s; };

Certain constructions can appear as both expressions and statements,
such as assignment or function calls.

However, in OCaml, there is no such distinction between expression
and statement; everything is treated as an expression.
Therefore, you can write: *)

1 + if x = 0 then 2 else 3

(* without encountering the expression/statement differentiation
found in imperative languages. *)

(* ----------------------------- Algebraic Data Types ------------------------*)

type binop = Add

(* Expressions: constants and binary operations (so far only addition) *)
type expr = Const of int | Binop of binop * expr * expr

(* Values we can introduce using global declarations
   "let name = ..."  *)
let one = Const 1
let two = Const 2
let ten = Binop (Add, Const 3, Const 7)
let e42 = Binop (Add, Binop (Add, Const 12, Const 20), Const 10)

(* we can redefine the expression e42 *)
let e42 = Binop (Add, Binop (Add, Const 12, Const 20), ten)

(* ------------------------------ Functions ----------------------------------*)
(* we can put the argument and return type explicitly *)
let pls_one (e : expr) : expr = Binop (Add, e, Const 1)

(* but we don't have to, OCaml will infer them for us. *)
let pls_one e = Binop (Add, e, Const 1)
let times_two e = Binop (Add, e, e)

(* ----------------------------- Pattern Matching ----------------------------*)
(* when we have an algebraic data type, we can make pattern matching analysis
   (of which constructor is the value of such a type).
   Here we declare a function that takes an arithmetic expression and
   returns a string saying that if expression e is a constant, we give it's
   string representation, and in case of addition we just return a string
   "addition" (we will of course redefine this function better just below). *)
let expr_to_string e =
  match e with Const c -> string_of_int c | Binop (Add, e1, e2) -> "Addition"

let _s1 = expr_to_string one
let _s2 = expr_to_string ten
let _s3 = expr_to_string (times_two one)
let _s4 = expr_to_string e42

(* --------------------------- Recursive Functions ---------------------------*)

(* Because the type of arithmetic expressions is recursive, our functions
   deconstructing those expressions are most of the time recursive as well:  *)
let rec expr_to_string e =
  match e with
  | Const c -> string_of_int c
  | Binop (Add, e1, e2) ->
      let s1 = expr_to_string e1 in
      let s2 = expr_to_string e2 in
      "(" ^ s1 ^ " + " ^ s2 ^ ")"

let _s1 = expr_to_string one
let _s2 = expr_to_string ten
let _s3 = expr_to_string (times_two one)
let _s4 = expr_to_string e42
(* returns the string "((12 + 20) + (3 + 7))" *)

(* --- Interpreter --- *)

(* Finally, let's interpret our arithmetic expressions, i.e.
   actually compute their values: *)
let rec interp e =
  match e with Const c -> c | Binop (Add, e1, e2) -> interp e1 + interp e2

let _ = interp one
let _ = interp ten
let _ = interp (times_two one)
let _ = interp e42
(* interp of ((12 + 20) + (3 + 7)) should indeed give 42. *)

(* --- Syntax Manipulation --- *)

(* Let's introduce some more examples where some parts of the
   expression are constants 0: *)

let v1 = Binop (Add, Const 0, Const 1)
let s1 = expr_to_string v1
(* val s1 : string = "(0 + 1)" *)

let v2 =
  Binop (Add, Const 0, Binop (Add, Const 0, Binop (Add, Const 4, Const 0)))

let s2 = expr_to_string v2
(* val s2 : string = "(0 + (0 + (4 + 0)))" *)

let v3 =
  Binop (Add, Binop (Add, Const 0, Const 6), Binop (Add, Const 0, Const 15))

let s3 = expr_to_string v3
(* val s3 : string = "((0 + 6) + (0 + 15))" *)

(* Let's simplify those expressions removing all zeros
   (This can be seen as a toy example of compiler optimisation. *)

(* Simplifier version 1, wrong -
   does not simplify in depth of the expression *)
let rec simplify e =
  match e with
  | Const c -> e
  | Binop (Add, Const 0, e) | Binop (Add, e, Const 0) -> e
  | Binop (Add, e1, e2) -> Binop (Add, e1, e2)

let s1 = expr_to_string (simplify v1)
let s2 = expr_to_string (simplify v2)
let s3 = expr_to_string (simplify v3)

(* Simplifier version 2, correct *)
let rec simplify e =
  match e with
  | Const c -> e
  | Binop (Add, Const 0, e) | Binop (Add, e, Const 0) -> simplify e
  | Binop (Add, e1, e2) -> Binop (Add, simplify e1, simplify e2)

let s1 = expr_to_string (simplify v1)
let s2 = expr_to_string (simplify v2)
let s3 = expr_to_string (simplify v3)

(* ----------------------------- Exceptions ----------------------------------*)

(* Declaring exception *)
exception Negative_Const

let rec check_valid e =
  match e with
  | Const c -> if c < 0 then raise Negative_Const else Const c
  | Binop (Add, e1, e2) -> Binop (Add, check_valid e1, check_valid e2)

let c = check_valid (Const (-1))

(* Declaring exception with a value *)
exception Negative_Const of int

let rec check_valid e =
  match e with
  | Const c -> if c < 0 then raise (Negative_Const c) else Const c
  | Binop (Add, e1, e2) -> Binop (Add, check_valid e1, check_valid e2)

let c = check_valid (Const (-1))

let print_expr e =
  try Printf.printf "%s\n" (expr_to_string (check_valid e))
  with Negative_Const c ->
    Printf.printf "Invalid input: the constant %d is a negative number\n" c

let () = print_expr (Const (-12))
