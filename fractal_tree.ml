open Graphics ;;
#load "graphics.cma" ;;

(** Types **)
type _root = R of int ;;
type _branch = N of float * float ;;
type ftree = Tree of _root * _branch;;

(** Global vars **)

let width = 400 and height = 400 ;;

let angle ref = 0. ;;

type log = float * float ;;

let tf = float_of_int and ti = int_of_float ;;

let pi = 3.141592 ;;

let degree (r: float) = (pi *. r ) /. 180. ;;

(** Functions **)
(* draw a window *)
let setScreen w h t =
	open_graph "";
	set_window_title t ;
	resize_window w h;;

(* set which color will be  on  another *)
let colorMode fg  bg =
	set_color bg;
	fill_rect 0 0 width height;
	set_color fg;;

(* trigonometric formula *)

let rotate (p: log) (a: float) (l:float) : log =
     (l *. sin (degree a)) , (l *. cos (degree a));;


let root (t: ftree) : log = match t with
	| Tree ( R(r) , _ ) -> (tf (width/2)) , 0. ;;

(* draw branch 's tree 1 then 2 then 4 .. *)
let rec branch (t: ftree) (itr: int) (prev: log ) = 
	match itr , t , prev with
	| 0 , _ , _ -> ()
	| e , Tree (R(r) , N(l,a)) , (x,y) ->

	let s,c = rotate (x,y) a l in
		let rx = x +. s and ry = y +. c in

	draw_segments [| ( (ti x) , (ti y), (ti rx), (ti  ry) ) |] ;

	branch (Tree (R(r) , N((l *. 0.67), (a +. !angle) ) )) (itr-1) (rx,ry);
	branch (Tree (R(r) , N((l *. 0.67), (a -. !angle) ) )) (itr-1) (rx,ry) ;;

(* bool for exit program *)
let exec = true ;;

(* exec main function untill i exit program *)
let rec loop main exec : unit =
	match exec with
	| true -> main ; loop main exec
	| _ -> () 

(* main function *)
let main : unit = 

	setScreen width height "Fractal Tree";
	(* the for change angle from 0 to 360 to show each fractal form *)
	for i=0 to 360 do
	colorMode white black ;
	(* define the tree *)
	let t = Tree( R(120) , N(80. , 0. ) ) in
	let p = root t in
	angle := !angle +. (tf i);
	branch t 10 p ; done ;; 

(** Exec **)
loop main exec ;;
