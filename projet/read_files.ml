(*Gere la lecture et la mise en "L-systeme" d'un fichier au format sys*)
open Systems
open Turtle
(*Fonction qui indique si on doit prendre en compte la ligne en argument ou
non, c'est-à-dire si ce n'est pas un commentaire ou une ligne de demarquation
entre l'axiome, les regles et l'interprétation'*)
let ligne_a_garder line =
	if line = "" then false
	else
	let x = String.get line 0 in
	 	(x <> '#') && (x <> '\n') && (x <> ' ')
(*Renvoie les composants du L-systeme décrit dans le fichier, à savoir l'axiome
*)
let open_file namefile =
	let chan = open_in namefile in
	(*recup_info explore les lignes du fichier et remplit les accumulateurs.
	Si on rencontre une ligne à passer (commentaire, ou ligne de demarquation(ligne vide)
	, on verifie si l'accumulateur courante (le n-ieme) est vide ou non. S'il est rempli,
	on passe à l'accumulateur suivant)*)
	let rec recup_info chan ax regle inter n last_line =
		(*Parcours du fichier*)
		try
			let line = input_line chan in
				(*Si la ligne est utile, on la passe à l'un des 3 accumulateurs, selon
				*)
				if (ligne_a_garder line)
				then
					if (n = 0)
					then recup_info chan line regle inter n true
					else if (n = 1)
					then recup_info chan ax (line::regle) inter n true
					else recup_info chan ax regle (line::inter) n true
				(*On passe au suivant seulement si l'accumulateur precedent est non vide*)
				else if (last_line)
				then
					recup_info chan ax regle inter (n+1) false
				(*Sinon on continue d'explorer le fichier*)
				else recup_info chan ax regle inter n false
		with
		End_of_file -> close_in chan;ax,List.rev regle,List.rev inter
	in
	let ax,regle,inter = recup_info chan "" [] [] 0 false in
		if (ax = "" || regle = [] || inter = [])
		then failwith "Fichier invalide"
		else
			ax,regle,inter
(*Convertit une chaine de caractere en une instance de word*)
let from_string_to_word s =
	if (String.length s = 1)
	then Symb s
	else
		(*On verifie si la séquence a des branches *)
		let index_b = String.index_opt s '[' in
 		match index_b with
 		| None ->
			let rec loop acc i n =
			if (i = n) then List.rev acc
			else
				let new_symb = Symb (Char.escaped (String.get s i)) in
					loop (new_symb::acc) (i+1) n
			in Seq (loop [] 0 (String.length s))
 		| Some b -> failwith "A faire"

let from_string_to_command s =
	match (String.get s 0) with
	| 'T' -> Turn (int_of_string (String.sub s 1 (String.length s)))
	| 'L' -> Line (int_of_string (String.sub s 1 (String.length s)))
	| 'M' -> Move (int_of_string (String.sub s 1 (String.length s)))
	| _ -> failwith "Interpretation invalide"
(* Ouvre un fichier et cree un Lsystem correspondant *)
let from_fich_to_syst namefile =
	let ax,regle,inter = open_file namefile in
		let axiom = from_string_to_word ax
		in
		let r = List.map (fun s -> String.split_on_char ' ' s) regle
		in
		let r_n = List.map (fun l -> (List.hd l,List.hd (List.tl l))) r
		in
		let i = List.map (fun s -> String.split_on_char ' ' s) inter
		in
		let i_n = List.map (fun l -> (List.hd l,List.hd (List.tl l))) i
		in
		{
			axiom;
			rules =
			(fun x ->
 				try
 					from_string_to_word (List.assoc x r_n)
 				with
 				| Not_found -> Symb x
			);
			interp =
			(fun x ->
				[from_string_to_command (List.assoc x i_n)])
		}