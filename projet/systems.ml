(** Words, rewrite systems, and rewriting *)
open Turtle
open Graphics
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
(*let rec list_command word lsyst =
	match word with
	| Symb s -> lsyst.interp s
	| Seq l -> List.fold_left (fun acc m -> (List.append acc (list_command m lsyst))) [] l
	| Branch w -> Store :: ((list_command w lsyst) @ [Restore]);;
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
*)
(*Types pour representer l'arbre utilise pour l'Interpretation a la volee*)
type 's foret =
	| Foret of 's arbre list
and 's arbre =
	| Node of 's foret * 's

(*Prend une chaine de caractère et renvoie la liste des caractères présents
dans cette chaine*)
let split_str s =
	let len = String.length s in
		let rec aux acc n len =
			if (n = len) then List.rev acc
			else
				let char_cour = String.get s n in
					aux (Char.escaped char_cour::acc) (n+1) len
		in
		aux [] 0 len
(*Renvoie la chaine de caractère correspondant à l'instance word en argument*)
let rec from_word_to_str w =
 	match w with
	| Symb a -> a
	| Seq l -> List.fold_left (fun a s -> a ^ from_word_to_str s) "" l
	| Branch b -> "[" ^ from_word_to_str b ^ "]"
(*Renvoie l'arbre correspondant a l'axiome du lsysteme en argument *)
let arbre_ax lsyst =
	let l_list = split_str (from_word_to_str lsyst.axiom) in
	 	Node(Foret(List.map (fun x -> Node(Foret([]),x)) l_list),"")
(*Renvoie l'arbre correspondant a l'iteration suivante de l'arbre en argument
dans le L-Systeme lsyst *)
let rec next_bis lsyst a =
	match a with
	| Node (f, s) ->
		begin match f with
		| Foret ([]) ->
			let l_s = split_str (from_word_to_str (lsyst.rules s)) in
				Node (Foret (List.map (fun s -> Node (Foret ([]), s)) l_s),s)
		| Foret (l) ->
				Node(Foret (List.map (fun n -> next_bis lsyst n) l), s)
		end
let iteration_bis n lsyst =
	let arbre = arbre_ax lsyst in
		let rec loop n t =
			if (n = 0) then t
			else
				loop (n-1) (next_bis lsyst t)
		in
		loop n arbre
(*Renvoie la commande a effectuer par rapport à la chaine de caractere correspondant
 au mot voulu*)
let exec s l_pos lsyst =
	match s with
	| "[" -> Store
	| "]" -> Restore
	| _ -> List.hd (lsyst.interp s);;
(*Exécute la commande cmd a la position renseignee dans l_pos*)
let exec_cmd cmd l_pos =
	let pos = List.hd l_pos in
		let pos_int = t_pos_to_pos pos 0
		in
			match cmd with
			| Move n | Line n ->
				let new_pos = t_pos_to_pos pos n in
					let new_pos_int = int_int_of_float_float new_pos in
						(line_or_move cmd) (fst new_pos_int) (snd new_pos_int);
						(pos_to_t_pos new_pos pos.a)::(List.tl l_pos)
			| Turn a ->
				((pos_to_t_pos pos_int (pos.a + a))::(List.tl l_pos))
			| Store ->
				(pos_to_t_pos pos_int pos.a)::l_pos
			| Restore ->
				(pos_to_t_pos pos_int pos.a)::l_pos
(*Execute les commandes relatives a l'arbre en argument*)
let rec exec_bis a pos lsyst =
	match a with
	| Node(Foret([]),s) ->
		let cmd = exec s pos lsyst in
				exec_cmd cmd pos
	| Node(Foret(n::l),s) ->
		let l_pos = exec_bis n pos lsyst in
				if (l <> []) then
				 exec_bis (Node(Foret(l),s)) l_pos lsyst
				else
					l_pos
