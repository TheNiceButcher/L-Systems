open Graphics
type command =
| Line of int
| Move of int
| Turn of int
| Store
| Restore

type position = {
  x: float;      (** position x *)
  y: float;      (** position y *)
  a: int;        (** angle of the direction *)
}
(*Type turtle qui regroupe les commandes à executer et une liste de position,
qui stocke la position courante en tête et les positions lors d'une commande
 Store*)
type turtle =
{
	cmds: command list; (*Les différentes commandes a executer*)
	pos: position list; (*La liste des positions à conserver, comme
						la position courante ou les positions lors d'un Store*)
}
(*Cree une instance de type tortue avec les arguments*)
let create_turtle cmd_list pos_list =
{
	cmds = cmd_list;
	pos = pos_list;
}
(*Change une position en (x,y) en une position de type Turtle.position*)
let pos_to_t_pos (x,y) a =
 	{
		x = x;
		y = y;
		a = a;
	}
(*Convertit une paire de float en une paire de int*)
let int_int_of_float_float (x,y) =
	int_of_float x,int_of_float y;;
(*Convertit un angle en degre en radiant*)
let deg_to_radian n =
 	let a = n mod 360 in
		let a' =
			if(a < 0) then (float_of_int a) +. 360.
			else float_of_int a in
		a' /. 180. *. 3.1415927

(*Retranscrit la position indiqué par le type position dans la position "reelle"*)
let t_pos_to_pos pos n =
	let angle = deg_to_radian pos.a in
	((pos.x +. float_of_int n *. (cos angle)),
	(pos.y +. float_of_int n *. (sin angle)))
(*Renvoie la position courante de la tortue*)
let pos_courante turtle =
	if (turtle.pos = []) then failwith "Liste pos vide"
	else
		List.hd (turtle.pos)
(*Fonction qui renvoie la fonction graphique à faire selon si la commande
est Line ou Move.
Cette fonction permet de traiter dans le match de exec_turtle ces deux cas de commandes
en meme temps*)
let line_or_move cmd =
 	match cmd with
	| Line _ -> lineto
	| Move _ -> moveto
	| _ -> failwith "line_or_move erreur"
(*Exécute les commandes de la torture en argument*)
let rec exec_turtle turtle =
	let pos = pos_courante turtle in
	let pos_int = t_pos_to_pos pos 0
	in
		match turtle.cmds with
		| [] -> () (*Fin de la liste des commandes de la tortue*)
		| c::l' ->
			match c with
			| Line n | Move n ->
				let new_pos = t_pos_to_pos pos n in
					let new_pos_int = int_int_of_float_float new_pos in
						(line_or_move c) (fst new_pos_int) (snd new_pos_int);
						exec_turtle (create_turtle l'
						((pos_to_t_pos new_pos pos.a)::(List.tl turtle.pos)));
			| Turn a ->
				exec_turtle (create_turtle l'
				((pos_to_t_pos pos_int (pos.a + a))::(List.tl turtle.pos)));
			| Store ->
				exec_turtle (create_turtle l'
				((pos_to_t_pos pos_int pos.a)::turtle.pos));
			| Restore ->
				exec_turtle (create_turtle l' (List.tl turtle.pos));
			;;
