
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
(*Demande le nom du fichier que l'utilisateur souhaite ouvrir*)
let rec recuperer_nom_fichier () =
 	let str = read_line() in
		try
			from_fich_to_syst str
		with
		| Sys_error _ | Failure _ ->
	print_string "Veuillez choisir un fichier valide\n";recuperer_nom_fichier ();;
(*Récupere la touche presse par l'utilisateur, avec en argument la dernière position*)
let rec recuperer_touche_utilisateur pos =
	let pos_int = int_int_of_float_float (t_pos_to_pos pos 0) in
	moveto 100 100;
	let s = "Appuyez sur A pour avancer d'une iteration,  R pour reculer et" ^
	"Q pour quitter la fenetre et le programme" in
	draw_string s;
	let e = wait_next_event [Key_pressed] in
	match e.key with
	| 'A' | 'a' -> moveto (fst pos_int) (snd pos_int);1
	| 'R' | 'r' -> moveto (fst pos_int) (snd pos_int);2
	| 'Q' | 'q' -> moveto (fst pos_int) (snd pos_int);-1
	| _ ->
	draw_string "Veuillez appuyer sur une touche valide";recuperer_touche_utilisateur pos;;
let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  	print_string "Bienvenue. Marquez le nom du fichier que vous voulez ouvrir\n";
	let sys = recuperer_nom_fichier () in
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
