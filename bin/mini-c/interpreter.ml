(* -------------------------------------------------------------------------- *)
(* Part O: Introduction *)
(* -------------------------------------------------------------------------- *)

(* The goal of this lab session is to build an LL(1) parser.
   This file is directly based on the material made by Jean-Christophe FilliÃ¢tre
   for his Compilers' course at Ã‰cole Normale SupÃ©rieure. *)

(* We start by defining the following OCaml types to define the notion of grammar:  *)

type terminal = string
type non_terminal = string
type symbol = Terminal of terminal | NonTerminal of non_terminal
type production = symbol list
type rule = non_terminal * production
type grammar = { start : non_terminal; rules : rule list }

(* Let's define the example from the lecture:

     E  ->  T E'
     E' ->  + T E'
         |  epsilon
     T  ->  F T'
     T' ->  * F T'
         |  epsilon
     F  -> ( E )
         | int

in OCaml using the grammar type above:
*)

let g_arith =
  {
    start = "S'";
    rules =
      [
        ("S'", [ NonTerminal "E"; Terminal "#" ]);
        ("E", [ NonTerminal "T"; NonTerminal "E'" ]);
        ("E'", [ Terminal "+"; NonTerminal "T"; NonTerminal "E'" ]);
        ("E'", []);
        ("T", [ NonTerminal "F"; NonTerminal "T'" ]);
        ("T'", [ Terminal "*"; NonTerminal "F"; NonTerminal "T'" ]);
        ("T'", []);
        ("F", [ Terminal "("; NonTerminal "E"; Terminal ")" ]);
        ("F", [ Terminal "int" ]);
      ];
  }

(* To define sets like NULL, FIRST, and FOLLOW from the lecture,
   we will need to manipulate the sets of non-terminal symbols.
   To do so, we start by defining the following OCaml module to represent
   sets of non-terminals: *)
module Ntset = Set.Make (String)

(* This gives access to various functions, e.g.
   Ntset.empty       --- create an empty set
   Ntset.singleton x --- create a singleton set
   Ntset.add x s     --- add non-terminal x to the set s
   Ntset.mem x s     --- check if x is in s
   Ntset.fold f s a  --- transform set s into another datatype using f and a
   etc.
*)

(* For example, we can compute the set of all non-terminals of a grammar using
   the two functions below, nts_of_production and non_terminals. *)
let nts_of_production pr =
  let add_symbol acc = function NonTerminal s -> Ntset.add s acc | _ -> acc in
  List.fold_left add_symbol Ntset.empty pr

let non_terminals (g : grammar) =
  let add_rule acc (ns, pr) =
    Ntset.union (nts_of_production pr) (Ntset.add ns acc)
  in
  List.fold_left add_rule Ntset.empty g.rules

(* To visualize the result, we can use the following code which
   defines a pretty-printer to print a given set of non-terminals: *)
let pp_non_terminal fmt s = Format.fprintf fmt "%s" s

let pp_iter iter pp_elt fmt =
  let first = ref true in
  iter (fun elt ->
      if not !first then Format.fprintf fmt ",@ " else first := false;
      pp_elt fmt elt)

let pp_non_terminals fmt =
  Format.fprintf fmt "@[%a@]" (pp_iter Ntset.iter pp_non_terminal)

let print_non_terminals nts =
  Format.printf "Non-terminals:\n   %a@." pp_non_terminals nts

(* We can test is on the grammar of arith expressions.x *)
let () = print_non_terminals (non_terminals g_arith)
(* Non-terminals:
   E, E', F, S', T, T' *)

(* -------------------------------------------------------------------------- *)
(* Part 1: Fixpoint *)
(* -------------------------------------------------------------------------- *)

(** Exercise 1.1. *)
(* To facilitate fixed point calculations in questions following,
   write a function val fixpoint : ('a -> 'a * bool) -> 'a -> 'a *)

(* *)
let rec fixpoint (f : 'a -> 'a * bool) (x : 'a) : 'a = assert false

(* Test of fixpoint : *)
let factorial m =
  let fact_aux (a, b) =
    if b <= 0 then ((a, b), false) else ((a * b, b - 1), true)
  in
  let res = fixpoint fact_aux (1, m) in
  fst res

let test = factorial 10
(* val test : int = 3628800 *)

(* -------------------------------------------------------------------------- *)
(* Part 2. Computing Null(Î±). *)
(* -------------------------------------------------------------------------- *)
(* let denote by nulls sets of non-terminals:  *)
type nulls = Ntset.t

(* Write a function is_null_production: nulls -> production -> bool which,
   given the set of non-terminals that can be derived into the empty word,
   determines whether a word can be derived into the empty word.
   HINTS:
   - (It's the NULL(Î±) from the lecture)
   - Use Htset.mem : Ntset.elt -> Ntset.t -> bool to write an auxilairy
     function that checks whether a non-terminal is inside the set of
     non-terminals
   - Use List.for_all and that function to define is_null_production.
*)

(** Exercise 2.1. *)
let rec is_null_production (ns : nulls) (p : production) : bool =
  assert false (* TODO *)

(* Some tests to check your implementation: *)
let nst = Ntset.add "T'" (Ntset.singleton "E'")
let test1 = is_null_production nst [ NonTerminal "E'"; NonTerminal "T'" ]

(* val test1 : bool = true *)
let test2 = is_null_production nst [ NonTerminal "E"; NonTerminal "T'" ]
(* val test2 : bool = false *)

(* Define a function null: grammar -> nulls which computes all the
   non-terminal nulls of a given grammar.
   HINTS:
   - Use the fixpoint function in the following way:

      let null g =
      let step nulls =
      ...we compute the new set of nulls and
         indicate by the Boolean if the result of the current step
         changed the result (so that we need compute more steps)
      in
      fixpoint step Ntset.empty
   - It is of course possible to use Ntset.equal to determine if the
     nulls set has changed, but it is also possible to be more efficient
     by using is_null_production only on the rules whose left member
     is not yet in the nulls and detecting in this case whether nulls
     should be modified.
   - Inside step definition you can use List.fold_left function.
*)

(** Exercise 2.2. *)
let null (g : grammar) : nulls = assert false (* TODO *)

(* Test of null *)
let () =
  let nulls_arith = null g_arith in
  Format.printf "null: %a@." pp_non_terminals nulls_arith
(* null: E', T' *)

(* -------------------------------------------------------------------------- *)
(* Part 3. Computing First(Î±). *)
(* -------------------------------------------------------------------------- *)
(* We define the following OCaml modules to represent sets of terminals (Tset)
   and dictionaries indexed by non-terminals (Ntmap): *)

module Tset = Set.Make (String)
module Ntmap = Map.Make (String)

(* The sets of first that we will compute in this question will therefore have
   the following type: *)

type firsts = Tset.t Ntmap.t

(* that is, firsts is a type of a dictionary which associates with each
   non-terminal symbol a set of terminal symbols. *)

(* Write a function val empty_map: grammar -> Tset.t Ntmap.t which constructs
   a dictionary associating each non-terminal of the grammar with an empty set
   of terminals. *)

(** Exericse 3.1 *)
let empty_map (g : grammar) : Tset.t Ntmap.t = assert false (* TODO *)

(* Again, some functions, to visualize the result and test solutions for the
   exercises that follow: *)
let pp_iter_bindings iter pp_binding fmt =
  let first = ref true in
  iter (fun key elt ->
      if not !first then Format.fprintf fmt "@\n" else first := false;
      pp_binding fmt key elt)

let pp_terminal fmt s = Format.fprintf fmt "%s" s

let pp_firsts fmt =
  Format.fprintf fmt "@[%a@]"
  @@ pp_iter_bindings Ntmap.iter (fun fmt nt ts ->
         Format.fprintf fmt "@[%a -> {%a}@]" pp_non_terminal nt
           (pp_iter Tset.iter pp_terminal)
           ts)

(* Write a fonction val first_production_step : nulls -> firsts -> production -> Tset.t
   that computes the set of firsts of a given production, given a set of non-terminals nulls
   and a dictionnary of the firsts for each non-terminal.
   HINTS:
   - It's the first(Î²) function from the lecture.
   - Use functions Ntmap.find, Ntset.mem, Ntset.union to
       -- Ntset.mem :  check if a non-terminal is in the set of nulls
       -- Ntmap.find : search for the firsts of a given non-terminal
          (you can use try ... with Not_found -> failwith "error_message ..."
           around the call to Ntmap.find.
       -- Ntset.union : to make a union of two sets *)

(** Exercise 3.2 *)
let rec first_production_step (nls : nulls) (fsts : firsts) (pr : production) :
    Tset.t =
  assert false
(* TODO *)

(* Write a function val first: grammar -> nulls -> firsts which computes the set of first
   non-terminals of a grammar, given the set of null non-terminals of this grammar
   (the latter being passed as an argument to avoid being recalculated several times).
   HINTS:
   - It's the first(X) function from the lecture.
   - We will use the fixpoint function and we will determine if the set of primes is modified
     using the Tset.subset function which determines the set inclusion. *)

(** Exercise 3.3 *)
let first (g : grammar) (nls : nulls) : firsts = assert false (* TODO *)

(* Test. *)
let () =
  let nulls_arith = null g_arith in
  let firsts_arith = first g_arith nulls_arith in
  Format.printf "first: %a@." pp_firsts firsts_arith
(* first: E -> {(, int}
       E' -> {+}
       F -> {(, int}
       S' -> {(, int}
       T -> {(, int}
       T' -> {*}
 *)

(* -------------------------------------------------------------------------- *)
(* Part 4. Computing Follow(Î±) *)
(* -------------------------------------------------------------------------- *)
(* Finally, we will compute the set of follow of the grammar.
   The set of follow is represented by the same OCaml type as first: *)

type follows = Tset.t Ntmap.t

(** Exercise 4.1 *)
(* Implement the function follow : grammar -> nulls -> firsts -> follows
   that computes the follows of a grammar, given the set of nulls and firsts.
   HINT: One can use the following code structure to implement follows:

   let follow g nulls firsts =
      let update (follows,b) nt follow_nt =
        ...
        ... updates the table of follow with nt -> follow_nt
        ... and replaces b by true if the table has been changed
        ...
     in
     let rec update_prod ((follows,b) as acc) nt p =
        ...
        ... analyzes the grammar production nt -> p
        ... and updates the pair (follows,b) for each non-terminal X of p
        ...
     in
     let step follows =
        List.fold_left
         (fun acc (nt,p) -> update_prod acc nt p)
         (follows,false) g.rules
     in
     fixpoint step (empty_map g) *)

let follow (g : grammar) (nuls : nulls) (fsts : firsts) =
  let update (follows, b) nt follow_nt =
    assert false
    (* TODO *)
  in
  let rec update_prod ((follows, b) as acc) nt p =
    assert false
    (* TODO *)
  in
  let step follows =
    List.fold_left
      (fun acc (nt, p) -> update_prod acc nt p)
      (follows, false) g.rules
  in
  fixpoint step (empty_map g)

(* To visualize the result, we can reuse the pp_firsts function for the set of follows: *)
let pp_follows = pp_firsts

(* Test: *)
let () =
  let nulls_arith = null g_arith in
  let firsts_arith = first g_arith nulls_arith in
  let follows_arith = follow g_arith nulls_arith firsts_arith in
  Format.printf "follow: %a@." pp_follows follows_arith
(* follow:
  E  -> {#, )}
  E' -> {#, )}
  F  -> {#, ), *, +}
  S' -> {}
  T  -> {#, ), +}
  T' -> {#, ), +}
*)

(* -------------------------------------------------------------------------- *)
(* Part 5. Constructing  LL(1) Expansion Table *)
(* -------------------------------------------------------------------------- *)

(* To represent the dictionaries indexed by terminals and the production sets,
   we define the following OCaml types: *)
module Tmap = Map.Make (String)

module Pset = Set.Make (struct
  type t = production

  let compare = compare
end)

(* We then define the following type for the top-down parser table: *)
type expansion_table = Pset.t Tmap.t Ntmap.t

(* Such a table is therefore a dictionary associating to each non-terminal symbol
    and then to each terminal symbol a set of productions.
    The table is sparse: when a row or column in the table is empty,
    there is no corresponding entry in the table. *)

(* Implement add_entry: expansion_table -> non_terminal -> terminal -> production -> expansion_table
   which adds an entry to the table. Note: Care must be taken to correctly handle the case of a
   first occurrence of a row or column. *)

(** Exericse 5.1 *)
let add_entry table nt t p = assert false
(* TODO *)

(* Write a function expansions: grammar -> expansion_table
    that computes the expansion table of top-down parser for a given grammar. *)

(** Exercise 5.2 *)
let expansions (g : grammar) : expansion_table = assert false (* TODO *)

(* To visualize the result, we can use the following code which defines a pretty-printer pp_table
   for the expansion tables: *)
let pp_symbol fmt = function
  | Terminal s -> Format.fprintf fmt "\"%s\"" s
  | NonTerminal s -> Format.fprintf fmt "%s" s

let rec pp_production fmt = function
  | [] -> ()
  | [ x ] -> pp_symbol fmt x
  | x :: l -> Format.fprintf fmt "%a %a" pp_symbol x pp_production l

let pp_table fmt t =
  let print_entry c p =
    Format.fprintf fmt "  %s: @[%a@]@\n" c pp_production p
  in
  let print_row nt m =
    Format.fprintf fmt "@[Expansions for %s:@\n" nt;
    Tmap.iter (fun c rs -> Pset.iter (print_entry c) rs) m;
    Format.fprintf fmt "@]"
  in
  Ntmap.iter print_row t

let table_arith = expansions g_arith
let () = Format.printf "%a@." pp_table table_arith

(*

Expansions for E:
  (: T E'
  int: T E'
Expansions for E':
  #:
  ):
  +: "+" T E'
Expansions for F:
  (: "(" E ")"
  int: "int"
Expansions for S':
  (: E "#"
  int: E "#"
Expansions for T:
  (: F T'
  int: F T'
Expansions for T':
  #:
  ):
  *: "*" F T'
  +:

 *)
(* We can compare it with the table seen in the lecture: *)
(*
    |    +    |    *    |    (     |    )    |   int   |   #   |
---------------------------------------------------------------
 E  |         |         |    TE'   |         |   TE'   |       |
---------------------------------------------------------------
 E' |   +TE'  |         |          |    â‚¬    |         |   â‚¬   |
---------------------------------------------------------------
 T  |         |         |    FT'   |         |   FT'   |       |
---------------------------------------------------------------
 T' |    â‚¬    |   *FT'  |          |   â‚¬     |         |   â‚¬   |
---------------------------------------------------------------
 F  |         |         |   (E)    |         |   int   |       |
---------------------------------------------------------------
*)

(* Another example: grammar characterizing words with same number of a's and b's *)
let g1 =
  {
    start = "S'";
    rules =
      [
        ("S'", [ NonTerminal "S"; Terminal "#" ]);
        ("S", []);
        ("S", [ Terminal "a"; NonTerminal "A"; NonTerminal "S" ]);
        ("S", [ Terminal "b"; NonTerminal "B"; NonTerminal "S" ]);
        ("A", [ Terminal "a"; NonTerminal "A"; NonTerminal "A" ]);
        ("A", [ Terminal "b" ]);
        ("B", [ Terminal "b"; NonTerminal "B"; NonTerminal "B" ]);
        ("B", [ Terminal "a" ]);
      ];
  }

let table1 = expansions g1
let () = Format.printf "%a@." pp_table table1
(*
Expansions for A:
  a: "a" A A
  b: "b"
Expansions for B:
  a: "a"
  b: "b" B B
Expansions for S:
  #:
  a: "a" A S
  b: "b" B S
Expansions for S':
  #: S "#"
  a: S "#"
  b: S "#"
 *)

(* -------------------------------------------------------------------------- *)
(* Part 6. Checking whether a word is recognized by an LL(1) Grammar *)
(* -------------------------------------------------------------------------- *)

(** Exercise 6.1 *)
(* Write a function is_ll1: expansion_table -> bool which determines if the
   expansion table contains at most one rule per case (which, by definition,
   then characterizes the grammar as belonging to the LL(1) class). *)

let is_ll1 (table : expansion_table) : bool = assert false (* TODO *)
let () = assert (is_ll1 table1)
let () = assert (is_ll1 table_arith)

(* Also test with a grammar of your choice that is not LL(1). *)

(* -------------------------------------------------------------------------- *)
(* Part 7. Checking whether a word is recognized by an LL(1) Grammar *)
(* -------------------------------------------------------------------------- *)

(* Implement function analyze : non_terminal -> expansion_table -> string list -> bool
   that decides if a word is recognized by a grammar, given its start non-terminal and
   expansion table. (We will not concern ourselves with whether the table corresponds
   to an LL(1) grammar, and in case of ambiguity, we will choose a production randomly
   with Pset.choose.) We will not forget to add the "#" symbol at the end of the word.

   Note: since we have added a rule S' -> S#, the analysis is successful if and only
   if we reach both an empty stack and an empty input list.

 *)

(** Exercise 7.1 *)
let analyze (nt : non_terminal) (t : expansion_table) (word : string list) :
    bool =
  assert false (* TODO *)

(* We can test with grammar g1 using the following code: *)
let explode s =
  let n = String.length s in
  let rec make i = if i = n then [] else String.make 1 s.[i] :: make (i + 1) in
  make 0

(* And we conclude with the following tests: *)
let test1 s = analyze g1.start (expansions g1) (explode s)
let () = assert (test1 "")
let () = assert (test1 "ab")
let () = assert (test1 "ba")
let () = assert (test1 "abab")
let () = assert (test1 "aaabbb")
let () = assert (test1 "aaabababbbababab")
let () = assert (test1 "abababbaabbbababaaaaabbbaaaabbbbababababaaaabbbbabab")
let () = assert (not (test1 "a"))
let () = assert (not (test1 "b"))
let () = assert (not (test1 "aab"))
let () = assert (not (test1 "aaabbba"))
let () = assert (not (test1 "aaabbbaabab"))
let () = assert (not (test1 "aaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb"))
