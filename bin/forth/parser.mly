%{
  (* Import Ast module from ast.ml *)
  open Ast
%}

// Declarations
%token EOF
%token <int> INT_CONSTANT
%token <float> FLOAT_CONSTANT
%token <string> WORD

// Start symbol type: Ast.program from ast.ml. Needs to be a fully qualified name despite import
%type <Ast.program> program
%start program

%%

// Rules
// Rule for parsing a program
program:
  | atom* EOF { Program $1 }
;

// Rule for parsing an atom
atom:
  | i = INT_CONSTANT { Int i }
  | f = FLOAT_CONSTANT { Float f }
  | s = WORD { Word s }
;
