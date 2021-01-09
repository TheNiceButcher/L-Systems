(** Words, rewrite systems, and rewriting *)
open Turtle
type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word

type 's rewrite_rules = 's -> 's word

type 's system = {
    axiom : 's word;
    rules : 's rewrite_rules;
    interp : 's -> Turtle.command list }

(*Renvoie la suite de symboles qui resulte de l'application de la substitution
du Lsysteme en argument  sur la suite de symbole en argument*)
let rec suivant word lsyst =
	match word with
	| Symb s -> lsyst.rules s
	| Seq l -> Seq (List.map (fun w -> suivant w lsyst) l)
	| Branch w -> Branch (suivant w lsyst);;
(*Applique i fois la substitution du Lsyteme en argument
 sur l'axiome de ce dernier et renvoie la suite de symboles correspondante*)
let iteration lsyst i =
	let mot = lsyst.axiom in
		if (i = 0) then mot
		else
			let rec next new_word j =
			 if (j = i) then new_word
			 else next (suivant new_word lsyst) (j+1)
			 in
			 next mot 0;;
(*Affiche la suite de symboles en argument*)
let rec afficher_chaine_symbole word =
	match word with
	| Symb s -> print_string s
	| Seq l ->
		begin match l with
			| [] -> print_string ""
			| w::ll -> (afficher_chaine_symbole w);(afficher_chaine_symbole (Seq (ll)));
			end
	| Branch w -> print_string "[";afficher_chaine_symbole w; print_string "]";;
(*Renvoie la liste de commande à effectuer d'après la liste de symboles et
l'Interpretation du Lsysteme en argument*)
let rec list_command word lsyst =
	match word with
	| Symb s -> lsyst.interp s
	| Seq l -> List.fold_left (fun acc m -> (acc @ (list_command m lsyst))) [] l
	| Branch w -> [Store] @ (list_command w lsyst) @ [Restore];;
let afficher_commande word lsyst =
	let list_c = list_command word lsyst in
		let rec aux l =
			match l with
			| [] -> ()
			| Line a::l1 -> print_string "Line ";print_int a;aux l1
			| Move a::l1 -> print_string "Move ";print_int a;aux l1
			| Turn a::l1 -> print_string "Turn ";print_int a;aux l1
			| Store::l1-> print_string "Store";aux l1
			| Restore::l1 -> print_string "Restore";aux l1
		in
		aux list_c;;
