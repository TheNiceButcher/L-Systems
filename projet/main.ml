
(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)
open Lsystems.Systems
open Lsystems.Turtle
let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"

let action_what () = Printf.printf "%s\n" usage; exit 0

let cmdline_options = [
("--what" , Arg.Unit action_what, "description");
]

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s)

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  let lsyst =
  	let axiom = Symb "A" in
		let rules s = match s with
		| "A" -> Seq [Symb "A";Branch (Seq [Symb "P";Symb "A"]);Symb "A";
		Branch (Seq [Symb "M";Symb "A"]);Symb "A"]
		| q -> Symb q
		in
		let interp s = match s with
					   | "A" -> [Line 10]
					   | "P" -> [Turn 60]
					   | "M" -> [Turn (-60)]
					   | _ -> failwith "Symbole introuvable"
		in
	{axiom;rules;interp}
	in
	afficher_chaine_symbole (iteration lsyst 1);
	afficher_commande (iteration lsyst 1) lsyst;
	print_string "\n";
  print_string "Pour l'instant je ne fais rien\n"

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
