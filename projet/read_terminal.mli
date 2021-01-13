open Systems
open Turtle
(*Type indiquant le choix de l'utilisateur par rapport au L-systeme courant*)
type action =
	Avancer | Reculer | ZoomAvant | ZoomArriere | Quitter;;
val recuperer_touche_utilisateur : position -> action
val menu_bienvenue : unit -> string system
