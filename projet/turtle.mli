
(** Turtle graphical commands *)
type command =
| Line of int      (** advance turtle while drawing *)
| Move of int      (** advance without drawing *)
| Turn of int      (** turn turtle by n degrees *)
| Store            (** save the current position of the turtle *)
| Restore          (** restore the last saved position not yet restored *)

(** Position and angle of the turtle *)
type position = {
  x: float;        (** position x *)
  y: float;        (** position y *)
  a: int;          (** angle of the direction *)
}
(*Type turtle qui regroupe les commandes à executer et une liste de position,
qui stocke la position courante en tête et les positions lors d'une commande
 Store*)
type turtle =
{
	cmds: command list;
	pos: position list;
}
val create_turtle: command list -> position list -> turtle
val exec_turtle: turtle -> unit
