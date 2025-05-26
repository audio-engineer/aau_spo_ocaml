/* Parser for While */

%{
  open Ast
%}

%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <string> IDENT
%token IF ELSE PRINT WHILE AND OR NOT
%token EOF
%token LP RP COMMA EQUAL COLON BEGIN END NEWLINE
%token PLUS MINUS TIMES DIV MOD

/* priorities and associativities */

%left OR
%left AND
%nonassoc NOT
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus

%start file
%type <Ast.file> file

%%

file:
| NEWLINE? b = nonempty_list(stmt) NEWLINE? EOF
    { Sblock b }
;

expr:
| c = CST
    { Ecst c }
| id = ident
    { Eident id }
| MINUS e1 = expr %prec unary_minus
    { Eunop (Uneg, e1) }
| NOT e1 = expr
    { Eunop (Unot, e1) }
| e1 = expr o = binop e2 = expr
    { Ebinop (o, e1, e2) }
| LP e = expr RP
    { e }
;

suite:
| s = simple_stmt NEWLINE
    { s }
| NEWLINE BEGIN l = nonempty_list(stmt) END
    { Sblock l }
;

stmt:
| s = simple_stmt NEWLINE
    { s }
| IF c = expr COLON s = suite
    { Sif (c, s, Sblock []) }
| IF c = expr COLON s1 = suite ELSE COLON s2 = suite
    { Sif (c, s1, s2) }
| WHILE e = expr COLON s = suite
    { Swhile (e, s) }
;

simple_stmt:
| id = ident EQUAL e = expr
    { Sassign (id, e) }
| id = ident PLUS EQUAL e = expr
    { Sassign (id, Ebinop (Badd, Eident id, e)) }
| PRINT LP el = separated_list(COMMA, expr) RP
    { Sprint el }
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV   { Bdiv }
| MOD   { Bmod }
| c=CMP { c    }
| AND   { Band }
| OR    { Bor  }
;

ident:
  id = IDENT { { loc = ($startpos, $endpos); id } }
;
