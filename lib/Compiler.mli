open VarState
open Cudd
open Core
open Wmc

(** Result of compiling an expression *)
type compiled_expr = {
  state: Bdd.dt btree;
  z: Bdd.dt;
  flips: Bdd.dt List.t}

type compiled_func = {
  args: (Bdd.dt btree) List.t;
  body: compiled_expr;
}


type compile_context = {
  man: Man.dt;
  name_map: (int, String.t) Hashtbl.Poly.t; (* map from variable identifiers to names, for debugging *)
  weights: weight; (* map from variables to weights *)
  lazy_eval: bool; (* true if lazy let evaluation *)
  free_stack: Bdd.dt Stack.t; (* a stack of unallocated BDD variables, for reuse *)
  funcs: (String.t, compiled_func) Hashtbl.Poly.t;
}

type compiled_program = {
  ctx: compile_context;
  body: compiled_expr;
}

(** Compile the input program to a [compiled_program] *)
val compile_program: CoreGrammar.program -> compiled_program

val get_prob: CoreGrammar.program -> Complex.t

exception Syntax_error of string

(** [parse_with_error] parses [lexbuf] as a program or fails with a syntax error *)
val parse_with_error: Lexing.lexbuf -> ExternalGrammar.program

(** [parse_and_prob]: [debug flag] -> [program text] -> [prob]
    Parses and prints the probability of [program text]. *)
val parse_and_prob: ?debug:bool -> string -> Complex.t

val parse_optimize_and_prob: ?debug:bool -> string -> Complex.t

(** prints the current position of the lex buffer to the out channel *)
val print_position : out_channel -> Lexing.lexbuf -> unit
