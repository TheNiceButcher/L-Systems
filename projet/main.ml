
open Lsystems (* Librairie regroupant le reste du code. Cf. fichier dune *)
open Systems (* Par exemple *)
open Turtle
open Graphics
open Examples
open Read_files

(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)
let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"

let action_what () = Printf.printf "%s\n" usage; exit 0

let cmdline_options = [
("--what" , Arg.Unit action_what, "description");
]

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s)
(*Demande le nom du fichier que l'utilisateur souhaite ouvrir et renvoie le
Lsysteme correspondant*)
let rec recup_syst_fich () =
 	let str = read_line() in
		try
			from_fich_to_syst str
		with
		| Sys_error _ | Failure _ ->
	print_string "Veuillez choisir un fichier valide\n";recup_syst_fich ();;
(*Cree un fichier au format sys avec l'axiome, les regles et les interpretations
en argument *)
let creation_fich ax ru inter =
	let fich = open_out ".creation_fich" in
		output_string fich ax;
		output_string fich "\n\n";
		let rec loop l =
		match l with
		| [] -> output_string fich "\n";
		| r::l' -> output_string fich r; output_string fich "\n"; loop l'
		in
		loop ru;
		loop inter;
		close_out fich;;
(*Recupere un Lsyteme donne par l'utilisateur via la ligne de commande*)
let rec recuperer_syst_read () =
	print_string "Donnez l'axiome de votre Lsystems :\n";
	let ax = read_line() in
	print_string "Donnez les regles du Lsystems (tapez '-1' quand vous avez fini)\n";
	print_string "Elles doivent etre sous la forme :\n";
	print_string "'s w' où s est le symbole dont w est la substitution de s\n";
	let rec loop i acc =
		if (i = (-1)) then List.rev acc
		else
			let r = read_line() in
			let j =
				try
					int_of_string r
				with
				| Failure _ -> 0
				in
					if (j = -1) then loop j acc
					else
						loop j (r::acc)
	in
	let ru = loop 0 [] in
	print_string "Donnez les interprétations du Lsystems (tapez '-1' quand vous avez fini)\n";
	let inter = loop 0 [] in
		creation_fich ax ru inter;
		try
		from_fich_to_syst ".creation_fich"
		with Failure _ ->
		begin
		print_string "Vous avez donne un Lsysteme invalide\nVeuillez recommencer";
		recuperer_syst_read()
		end;;


(*Récupere la touche presse par l'utilisateur, avec en argument la dernière position*)
let rec recuperer_touche_utilisateur pos =
	let pos_int = int_int_of_float_float (t_pos_to_pos pos 0) in
	moveto 100 100;
	let s = "Appuyez sur A pour avancer d'une iteration,  R pour reculer et" ^
	" Q pour quitter la fenetre et le programme" in
	draw_string s;
	let e = wait_next_event [Key_pressed] in
	match e.key with
	| 'A' | 'a' -> moveto (fst pos_int) (snd pos_int);1
	| 'R' | 'r' -> moveto (fst pos_int) (snd pos_int);2
	| 'Q' | 'q' -> moveto (fst pos_int) (snd pos_int);-1
	| _ -> recuperer_touche_utilisateur pos;;
let rec menu_bienvenue () =
	print_string ("Bienvenue\n"
	^ "A partir de quel source voulez-vous fourni le L-Systeme ?\n" ^
	"Tapez 1 pour un fichier au format .sys ou 2 pour le fournir via l'invite
	de commande\n");
	let choix = read_line() in
		let n =
		try
		 	int_of_string choix
		with Failure _ -> 0
		in
			match n with
			| 1 -> recup_syst_fich()
			| 2 -> recuperer_syst_read()
			| _ -> menu_bienvenue ()
let main () =
  Arg.parse cmdline_options extra_arg_action usage;
	let sys = menu_bienvenue () in
	open_graph " 800x800";
	(*On cree une boucle, qu'on arrete des que l'utilisateur tape q sur la fenetre*)
	let rec loop i =
		if (i = -1) then
		begin clear_graph(); close_graph(); end
		else
			let a_n = iteration_bis i sys
			in
				moveto 200 200;
				let l_pos = exec_bis a_n [{x=200.;y=200.;a=0}] sys in
				let n = recuperer_touche_utilisateur (List.hd l_pos) in
				match n with
				| 1 -> clear_graph();loop (i+1)
				| 2 when i <> 0 -> clear_graph();loop (i-1)
				| 2 -> loop 0
				| -1 -> loop n
				| _ -> failwith "Impossible"
	in
		loop 0;
	print_string "Au revoir";;

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
