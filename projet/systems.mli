(** Words, rewrite systems, and rewriting *)
type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word

type 's rewrite_rules = 's -> 's word

type 's system = {
    axiom : 's word;
    rules : 's rewrite_rules;
    interp : 's -> Turtle.command list }
(*Type arbre representant l'arbre *)
type 's foret =
	| Foret of 's arbre list
and 's arbre =
	| Node of 's foret * 's
val suivant : 's word -> 's system -> 's word
val iteration : 's system -> int -> 's word
(*val list_command: 's word -> 's system -> command list*)
val next_bis : string system -> string arbre -> string arbre
val arbre_ax : string system -> string arbre
val exec_bis : string arbre -> Turtle.position list -> string system -> Turtle.position list
val iteration_bis : int -> string system -> string arbre 
