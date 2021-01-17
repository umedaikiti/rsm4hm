%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <float>  FLOAT
%token <string> ID
%token SKIP SEMICOLON IF THEN ELSE FI WHILE DO OD ASSUME ASSERT TRUE FALSE
%token ASSGN NDET
%token PLUS STAR MINUS AND OR
%token INT REAL LSQBRACKET COMMA RSQBRACKET  LT GT LEQ GEQ EQ NOT PROB LPARAN RPARAN DOLLAR LBRACKET RBRACKET
%left SEMICOLON 
%left PLUS MINUS  OR AND
/* %token <string> DIST */
%token EOF

%start program
%type <Syntax.prog> program
%% 

program:
  | global stmt EOF { Prog ($1,$2) }
  | stmt EOF { Prog (BexpOr (AffUnit,BexpUnit),$1) }
;

stmt:
  | assgn { StAssgn $1 }
  | SKIP { StSkip }
  | LPARAN stmt RPARAN { $2 }
  | stmt SEMICOLON stmt { StSeq ($1, $3) }
  | IF ndbexpr THEN stmt ELSE stmt FI { StIf ($2, $4, $6) }
  | WHILE bexpr DO stmt OD { StWhile ($2, $4) }
  | LBRACKET bexpr RBRACKET { StAssume $2 }
  | ASSUME bexpr { StAssume $2 }
  | LBRACKET bexpr RBRACKET stmt { StSeq(StAssume $2, $4) }
  | ASSERT bexpr { StAssert $2 }
;

assgn:
  | pvar ASSGN expr { AsnDet ($1, $3) }
  | pvar ASSGN NDET dom  { AsnNdet ($1, $4) }
  | pvar ASSGN dist { AsnProb ($1, $3) }
;

expr:
  | const { EConst $1 }
  | pvar { EVar $1 }
  | const STAR pvar { EScalarMult ($1, $3) }
  | LPARAN expr RPARAN { $2 }
  | expr PLUS expr { EAdd ($1, $3) }
  | expr MINUS expr { ESub ($1, $3) }
;

dom:
  | LPARAN dom RPARAN { $2 }
  | dom_basis { $1 }
  | dom_basis OR dom { DOr ($1, $3) }
;
dom_basis:
  | INT { DInt }
  | REAL { DReal }
  | INT LSQBRACKET const COMMA const RSQBRACKET { DIntInterval ($3, $5) }
  | REAL LSQBRACKET const COMMA const RSQBRACKET { DRealInterval ($3, $5) }
;

bexpr:
  | LPARAN bexpr RPARAN { $2 } 
  | affexpr { BexpOr ($1,BexpUnit) }
  | bexpr OR bexpr { (*BexpOr ($1,$3) }*)
        let rec append_bexpr l r= match l with
          | BexpUnit -> r | BexpOr(lhd,ltl) -> BexpOr(lhd,append_bexpr ltl r) in
        append_bexpr $1 $3 } 
;

affexpr:
  | expr EQ expr { AffAnd (LitLeq ($1, $3), AffAnd(LitLeq ($3, $1),AffUnit)) }
  | literal { AffAnd ($1,AffUnit) }
  | affexpr AND affexpr { (*AffAnd ($1,$3) }*)
        let rec append_affexpr l r = match l with
          | AffUnit -> r | AffAnd(lhd,ltl) -> AffAnd(lhd,append_affexpr ltl r) in
        append_affexpr $1 $3 } 
  | LPARAN affexpr RPARAN { $2 } 
;

literal:
  | TRUE { LitLeq (EConst (Const 0.), EConst (Const 1.)) }
  | FALSE { LitLeq (EConst (Const 1.), EConst (Const 0.)) }
  | expr LEQ expr { LitLeq ($1, $3) }
  | expr GEQ expr { LitGeq ($1, $3) }
  | expr LT expr { LitNot (LitLeq ($3, $1)) }
  | expr GT expr { LitNot (LitGeq ($3, $1)) }
  | NOT literal { LitNot $2 }
;

ndbexpr:
  | STAR { Ndet_branch }
  | PROB LPARAN const RPARAN { Prob_branch $3 }
  | bexpr { Det_branch $1 }
;

pvar:
  | ID { PVar $1 }
;

dist:
  | ID LPARAN const_list RPARAN { Dist ($1, $3) }
;

const_list:
  | const { [$1] }
  | const COMMA const_list { $1 :: $3 }
;

const:
  | FLOAT { Const $1 }
;

global:
  | DOLLAR bexpr { $2 }
;

