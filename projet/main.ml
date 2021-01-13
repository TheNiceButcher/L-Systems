
open Lsystems (* Librairie regroupant le reste du code. Cf. fichier dune *)
open Systems (* Par exemple *)
open Turtle
open Graphics
open Examples
open Read_files
open Read_terminal

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

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
	let sys = menu_bienvenue () in
	open_graph " 800x800";
	(*On cree une boucle, qu'on arrete des que l'utilisateur tape q sur la fenetre*)
	let rec loop i syst =
		if (i = -1) then
		begin clear_graph(); close_graph(); end
		else
			let a_n = iteration i sys
			in
				moveto 200 200;
				let l_pos = exec_syst a_n [{x=200.;y=200.;a=0}] sys in
				let n = recuperer_touche_utilisateur (List.hd l_pos) in
				match n with
				| Avancer -> clear_graph();loop (i+1) syst
				(*Si on veut reculer, il faut que i > 0*)
				| Reculer when i <> 0 -> clear_graph();loop (i-1) syst
				| Reculer -> loop 0 syst
				| ZoomAvant -> loop i syst
				| ZoomArriere -> loop i syst
				| Quitter -> loop (-1) syst
	in
		loop 0 sys;
	print_string "Au revoir";;

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
