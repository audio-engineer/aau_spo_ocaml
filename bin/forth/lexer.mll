{
  open Lexing
  open Parser
}

(* Regex definitions *)
let digit = ['0'-'9']
let sign = ['-' '+']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = sign? digit+

let exponent = ['e' 'E']
let float_constant = sign? digit+ '.' digit+ (exponent sign? digit+)?
let identifier = alpha (alpha | digit | '-')*

let whitespace = [' ' '\t']+

(* Rules *)
rule token = parse
  (* If the int_constant regex matches the input, take the string matched by the regex,
  convert the string to an integer, and return the INT_CONSTANT token with said integer as the argument.
  INT_CONSTANT is a function that takes an int and returns a token. *)
  | int_constant { INT_CONSTANT (int_of_string (lexeme lexbuf)) }
  | float_constant { FLOAT_CONSTANT (float_of_string (lexeme lexbuf)) }
  | identifier { WORD (lexeme lexbuf) }
  | whitespace { token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ lexeme lexbuf ^ "'")) }
