(*Gere la lecture et la mise en "L-systeme" d'un fichier au format sys*)
open Systems
(*Fonction qui indique si on doit prendre en compte la ligne en argument ou
non, c'est-à-dire si ce n'est pas un commentaire ou une ligne de demarquation
entre l'axiome, les regles et l'interprétation'*)
let ligne_a_garder line =
	if line = "" then false
	else
	let x = String.get line 0 in
	 	(x <> '#') && (x <> '\n') && (x <> ' ')
(*REnvoie les composants du L-systeme décrit dans le fichier, à savoir l'axiome
*)
let open_file namefile =
	let chan = open_in namefile in
	(*recup_info explore les lignes du fichier et remplit les accumulateurs*)
	let rec recup_info chan ax regle inter n last_line =
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
(*Renvoie le mot correspondant à la chaine de caractère en argument*)
(*let string_to_word str =
	let list_char = List.split_on_char "" str in
	if (List.length list_char = 1)
	then Symb (List.hd list_char)
	else

let creation_lsyst ax regle inter =
	let axiome =
		if (String.length ax = 1)
		then
			Symb (String.get ax 0)
		else
			let
*)
