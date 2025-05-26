(* The goal is to transform the abstract syntax tree obtained from the Parser
   (Ptree.file) into a typed abstract syntax tree (Ttree.file).
   That is, (as stated in the typing.mli interface file) we want to implement
   a function val program: debug:bool -> Ptree.file -> Ttree.file *)

(* Prior to completing this lab, please read carefully and get familiar with:
   - the accompanying PDF describing the syntax and static typing of mini-C
   - Parsed Abstract Syntax Tree interface (Ptree.file)
   - Typed Abstract Tree interface (Ptree.file)
   - OCaml: manipulating List, Hashtbl, and Map.Make(String) modules,
     in particular List.iter, List.fold_left, etc. Also Pay attention to the
     fact that Hashtbl.t is a mutable type while Maps are immutable.
*)


(* -------------------------------------------------------------------------- *)
(* ----    Part 0.0. Utilities: exceptions, type equivalence, etc.      ----- *)
(* -------------------------------------------------------------------------- *)

(* We start by opening the typed expressions. *)
open Ttree

(* We define string representation for each type. *)
let string_of_type = function
  | Tint       -> "int"
  | Tstructp x -> "struct " ^ x.str_name ^ " *"
  | Tvoidstar  -> "void*"

(* We declare various typing errors that we want to detect during
   the type-checking phase. *)
exception Error of Ptree.loc option * string

let error ?loc s = raise (Error (loc, s))
let unbound_var x = error ("unbound variable " ^ x)
let unbound_fun x = error ("unbound function " ^ x)
let unbound_struct x = error ("unbound structure " ^ x)
let duplicated_field x = error ("duplicate " ^ x)
let incompatible_types t1 t2 =
  error ("incompatible types " ^
	 string_of_type t1 ^ " and " ^ string_of_type t2)
let bad_arity ?loc p a =
  error ?loc ("bad arity: function p expects " ^
              string_of_int a ^ " arguments")

(* We also define function val unique : string -> string
   that generates variables with fresh names using an internal counter.
   E.g. the 10-th call to 'unique "x" will return "x__10". *)
let unique =
  let r = ref 0 in fun s -> incr r; s ^ "__" ^ string_of_int !r

(* -------------------------------------------------------------------------- *)
(* --  Part 0.1. Global (struct, fun) and local (var) typing environments  -- *)
(* -------------------------------------------------------------------------- *)

(* We will use hash tables to represent global typing environments to
   typecheck declared structures and functions and we will use Maps
   to represent local (variable) typing environments.
   That is, in the description PDF we use the same Gamma to denote typing
   enviroments, but in actual implementation we will split Gamma into
   those three typing environments (global structs type declarations, global
   functions type declarations, local variable types).
*)

(* First, we define an hash table to keep track of declared structures
   and an hash table to keep track of declared functions. *)
let structures = Hashtbl.create 17
let funs = Hashtbl.create 17

(* That is,
   - `structures` table will associate a name of the structure `x`
      with Ttree.structure, i.e. with a value of type
      { str_name = x;
        str_size = "length of fields";
        str_fields = "hash table from field names to field types"  }
   - `funs` table will associate a name of the function `f` with
      with its return type, and the types of formal parameters. *)

(* We start by adding predefined functions into funs environment: *)
let () =
  Hashtbl.add funs "putchar" ("putchar", Tint, [Tint, "x"]);
  Hashtbl.add funs "malloc" ("malloc", Tvoidstar, [Tint, "x"])

(* and we can check if the function id is already added to the table: *)
let is_global id =
  Hashtbl.mem funs id

(* Finally, we define the OCaml type of typing environments `ty_env` that
   associates variable names with pairs of their types and their unique representation
   (which is needed to typecheck local variables inside a single block). *)
module Env = Map.Make(String)
type typ_env = (Ttree.typ * string) Env.t

let ident env x =
  try Env.find x env
  with Not_found -> unbound_var x

(** Exercise 1. Implement the type equivalence relation
   val eq_type: Ttree.typ -> Ttree.typ -> bool *)
let eq_type t1 t2 = assert false (* TODO *)



(* -------------------------------------------------------------------------- *)
(* --    Part 1. Adding struct declarations to the typing environment      -- *)
(* -------------------------------------------------------------------------- *)

(** Exercise 2. Implement typ_of_ptyp function

   val typ_of_ptyp: Ptree.typ -> Ttree.typ

   that turns types from parsed trees into types of typed trees.
   In case of the structure pointer type the function should check that
   the type has been already added to the structures hash table and
   if it is not found, it should raise unbound_struct exception. *)
let typ_of_ptyp = function
  | Ptree.Tint -> assert false (* TODO *)
  | Ptree.Tstructp id -> assert false (* TODO *)

(** Exercise 3. Implement check_unique : (Ptree.typ * Ptree.ident) list -> unit
   that checks the uniqueness of the fields in the structure declaration.
   If there are two fields with the same name x, the function should raise the
   exception `duplicated_field x`;
   Hint: you can use a locally defined small-size hash table to add and check the
   uniquement of the fields. *)
let check_unique (vl : (Ptree.typ * Ptree.ident) list) =
  assert false (* TODO *)

(** Exercise 4. Finally, implement a function
   val decl_struct : Ptree.ident * (Ptree.typ * Ptree.ident) list -> unit
   that takes a structure identifier and the associated list of fields,
   checks the uniqueness of the fields and turns the declaration into
   a Ttree.structure value which it adds to the hash table `structures`.
   Hints:
   - use a locally-defined hash table to create a mapping from
     field name to its type and its offset.
   - use a local counter (let ofs = ref 0 in ...) to increment its
     value by 1 for each added field.
   - define a local function add_field (ty, {Ptree.id=x}) that
     uses those local data to process a given field (identified by a pair of
     field's type and field's id)
   - you can then conclude the implementation simply by List.iter add_field fl
*)
let decl_struct ({Ptree.id=x}, fl) : unit =
  (* check that structure is not yet has been declared; *)
  (* Check the uniqueness of the declared fields;  *)
  (* Define local mutable data (hash table and counter);  *)
  (* Define add_field (ty, {Ptree.id=x}) auxiliary function;  *)
  (* Conclude with List.iter add_field fl. *)
  assert false (* TODO *)


(* -------------------------------------------------------------------------- *)
(* ---------      Part 2. Type checking expressions                 --------- *)
(* -------------------------------------------------------------------------- *)

(** Exercise 5. Implement a function

    val expected_type: Ttree.typ -> Ttree.expr -> unit

    that takes a type t1 and a typed expression e2 and checks that the type
    of e2 is compatible with the  expected type t1, in the sense that those
    types are equivalent. The function returns unit value () in case of
    compatibility, and otherwise raises the incompatible_types exception.

    Hint: pay attention to the special case of 0l having two possible types
    (we use 0l because we use OCaml 32-bit signed integers in this lab).  *)
let expected_type t1 e2 : unit = match t1, e2 with
  | _, _ ->  assert false (* TODO *)

(** Exercise 6. Implement the function

   val decl_var: typ_env -> Ptree.typ * Ptree.ident -> (Ttree.typ * string) Env.t

   that takes typing environment and a pair of the declared type `ty` and
   variable `x` and extends the typing environment with association `x --> v`
   where `v` is a pair of the type made from `ty` and the fresh variable made
   from `x`. *)
let decl_var (env : typ_env) (pty, {Ptree.id=x}) =
  assert false

(** Exercise 7-9. Implement the auxiliary functions

    val expr_node: typ_env -> Ptree.expr_node -> Ttree.expr_node * Ttree.typ
    val arrow: typ_env -> Ptree.expr -> Ttree.ident -> Ttree.expr * int * Ttree.typ
    val call: typ_env -> Ptree.ident * Ptree.expr list -> Ttree.expr_node * Ttree.typ

    used in the expr function below and that typechecks
    the expression in the typing environment according to the
    typing rules presented in the accompanying pdf (section 3.2).

    Hints:
    - the cases of Earrow, Eassign_field, and Ecall are implemented
      using corresponding auxiliary mutually recursively functions
      that you should implement (Exercises 8 and 9).
    - remember to perform type checks when it is necessary using
      expected_type function from the exercises above. *)
let rec expr (env : typ_env) e =
  let te, ty = expr_node env e.Ptree.expr_node in
  { expr_node = te; expr_typ = ty }

(** Exercise 7. *)
and expr_node (env : typ_env) = function
  | Ptree.Econst n ->
    assert false (* TODO *)
  | Ptree.Eunop (Ptree.Uminus, e) ->
    assert false (* TODO *)
  | Ptree.Eunop (Ptree.Unot, e) ->
    assert false (* TODO *)
  | Ptree.Ebinop (Ptree.Beq | Ptree.Bneq as _op , e1, e2) ->
    assert false (* TODO *)
  | Ptree.Ebinop (Ptree.Blt | Ptree.Ble | Ptree.Bgt | Ptree.Bge |
		  Ptree.Badd | Ptree.Bsub | Ptree.Bmul | Ptree.Bdiv |
		  Ptree.Band | Ptree.Bor as _op , e1, e2) ->
    assert false (* TODO *)
  | Ptree.Esizeof {Ptree.id=x} ->
    assert false (* TODO; Hint: use Hashtbl.find structures *)
  | Ptree.Eident {id=x} ->
    assert false (* TODO; Hint: use ident function defined above *)
  | Ptree.Eassign_local ({Ptree.id = x}, e2) ->
    assert false (* TODO; Hint: use ident function defined above *)
  | Ptree.Emalloc {id=x} ->
    (try
       let s = Hashtbl.find structures x in
       Emalloc s, Tstructp s
     with Not_found ->
       unbound_struct x)
  | Ptree.Earrow (e, {id=x}) ->
    let (e1n, ofs, ty) = arrow env e x (* TODO: see Exercise 8 *) in
    Eaccess_field (e1n, ofs), ty
  | Ptree.Eassign_field (e1, {Ptree.id = x}, e2) ->
    let (e1n, f_ofs, ty) = arrow env e1 x (* TODO: see Exercise 8 *) in
    let e2t = expr env e2 in
    let e1t = { expr_node = Eassign_field (e1n, f_ofs, e2t); expr_typ = ty} in
    expected_type ty e2t;
    Eassign_field (e1t, f_ofs, e2t), ty
  | Ptree.Ecall ({id=x; id_loc=loc}, el) ->
    call env ({Ptree.id=x; id_loc=loc}, el) (* TODO: see Exercise 9 *)


(** Exercise 8. Implement
    val arrow: typ_env -> Ptree.expr -> Ttree.ident -> Ttree.expr * int * Ttree.typ *)
and arrow env e1 x =
  (* Type-check expression e1 to get to turn it into a typed one; *)
  (* Check that the corresponding obtained type
     which must be a pointer to a structure; *)
  (* Use field hash table to obtain field and offset data
     and return it together with the typed expression. *)
  assert false (* TODO *)

(* auxiliary function used in typing function calls. *)
and formal env (ty,_) e =
  let e = expr env e in
  expected_type ty e;
  e

(** Exercise 9. Implement
    val call: typ_env -> Ptree.ident * Ptree.expr list -> Ttree.expr_node * Ttree.typ
    Remember to check that formal params and actual arguments arity match. *)
and call env ({id=x; id_loc=loc}, el) =
  assert false (* TODO *)



(* -------------------------------------------------------------------------- *)
(* ---------      Part 3. Type checking statements                  --------- *)
(* -------------------------------------------------------------------------- *)
(* Now that we type-checked expressions, we can implement type-checking of the
   statements, following the typing rules described in the accompanying pdf
   (section 3.3). *)

(** Exercise 10-11. Implement functions

   val stmt: typ_env -> Ptree.stmt -> Ttree.stmt

   and

   val block: typ_env -> Ptree.block -> Ttree.block

    that turn parsed tree statements and blocks into the corresponding
    typed tree statements and blocks. *)

(** Exercise 10. *)
let rec stmt env s = match s.Ptree.stmt_node with
  | Ptree.Sskip ->
    assert false (* TODO *)
  | Ptree.Sexpr e ->
    assert false (* TODO *)
  | Ptree.Sif (e, s1, s2) ->
    assert false (* TODO *)
  | Ptree.Swhile (e, s) ->
    assert false (* TODO *)
  | Ptree.Sblock b ->
    assert false (* TODO *)
  | Ptree.Sreturn e ->
    assert false (* TODO *)

(** Exercise 11. Implement
    val block: typ_env -> Ptree.block -> Ttree.block
    Hints: start by extending local typing with local variables and their declared types
    using List.fold_left and decl_var functions.
*)
and block env (vl, sl) =
  assert false (* TODO *)

(* -------------------------------------------------------------------------- *)
(* ----        Part 4. Type checking functions and the program           ---- *)
(* -------------------------------------------------------------------------- *)

(** Exercise 12. Finally, implement the function

    val decl_fun: Ptree.decl_fun -> Ttree.decl_fun

    that turns a parsed tree function declaration into typed one.
    Remember to perform necessary typing and uniqueness checks, and to add the
    typed function to the funs typing environment.
*)
let decl_fun f =
  (* Check that the function name has not been added to funs hash table; *)
  (* Check uniqueness of formal parameters; *)
  (* Make a local typing environment by adding formal parameters with their
     declared types; *)
  (* Add function and its typing data to the global funs hash table; *)
  (* Use these data to return Ttree.decl_fun value. *)
  assert false (* TODO *)


(* Auxiliary function processing function and struct declarations
   and adding the result of typing phase to the accumulator acc. *)
let decl acc = function
  | Ptree.Dstruct s ->
      decl_struct s; acc
  | Ptree.Dfun f ->
      decl_fun f :: acc

let is_main f = f.fun_name = "main"

(* Main Typing function. *)
let program ~debug p =
  let fl = List.fold_left decl [] p in
  if not (debug || List.exists is_main fl)
  then error "missing main function";
  { funs = fl }
