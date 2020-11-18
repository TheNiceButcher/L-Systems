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

(** Put here any type and function implementations concerning systems *)
let rec suivant word lsyst =
	match word with
	| Symb s -> lsyst.rules s
	| Seq l -> Seq (List.map (fun w -> suivant w lsyst) l)
	| Branch w -> Branch (suivant w lsyst);;
let iteration lsyst i =
	let mot = lsyst.axiom in
		if (i = 0) then mot
		else
			let rec next new_word j =
			 if (j = i) then new_word
			 else next (suivant new_word lsyst) (j+1)
			 in
			 next mot 0;;
let rec afficher_chaine_symbole word =
	match word with
	| Symb s -> print_string s
	| Seq l -> begin match l with
				| [] -> print_string ""
				| w::ll -> (afficher_chaine_symbole w);(afficher_chaine_symbole (Seq (ll)));
				end
	| Branch w -> print_string "[";afficher_chaine_symbole w; print_string "]";
(*let list_command word lsyst =
	let rec aux l =
		match word with
		| Symb s -> List.rev (interp x)::l
		| Seq li -> begin match li with
						 | [] -> List.rev l
						 |
		| Branch w -> let
*)
