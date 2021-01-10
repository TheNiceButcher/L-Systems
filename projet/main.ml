
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

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  print_string "Bienvenue. Marquez le nom du fichier que vous voulez ouvrir\n";
  let str = read_line() in
	let snow_sys = from_fich_to_syst str in
	open_graph " 800x800";
	moveto 400 400;
	let a_n = iteration_bis 0 snow_sys
	in
	let _ = exec_bis a_n [{x=400.;y=400.;a=0}] snow_sys in
	Unix.sleep 10;
	clear_graph();;

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
