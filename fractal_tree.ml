open Graphics ;;
#load "graphics.cma" ;;
#load "unix.cma" ;;

let fps = 5. ;;

(** Types **)
type _root = R of int ;;
type _branch = N of float * float ;;
type ftree = Tree of _root * _branch;;

(** Global vars **)

let width = 400 and height = 400 ;;

let angle = ref 0. ;;

type log = float * float ;;

let tf = float_of_int and ti = int_of_float ;;

let degree (r: float) = let pi = 3.14 in (pi *. r ) /. 180. ;;

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
(* p is the last point i had drawed a branch before i use it to draw other and update it *)
let rec branch (t: ftree) (itr: int) (p: log ) = 
	match itr , t , p with
	| 0 , _ , _ -> ()
	| e , Tree (R(r) , N(l,a)) , (x,y) ->

	let s,c = rotate (x,y) a l in
		let rx = x +. s and ry = y +. c in

	draw_segments [| ( (ti x) , (ti y), (ti rx), (ti  ry) ) |] ;

	branch (Tree (R(r) , N((l *. 0.67), (a +. !angle) ) )) (itr-1) (rx,ry);
	branch (Tree (R(r) , N((l *. 0.67), (a -. !angle) ) )) (itr-1) (rx,ry) ;;

module Time = struct

let sec = Unix.time() ;;
let msec = Unix.gettimeofday() *. 1000. ;;

let wait (ms : float) : unit =
	let r = msec in
	let t = ref r and t_old = ref r in
		while !t < (!t_old +. (10000. /. ms) ) do 
			t := !t +. 0.001 ;
			done ;;
end;;

(* bool for exit program *)
let setScreen =
  open_graph "";
  resize_window width height;
  set_window_title "Fractal Tree" ;;
  
let exec = 
	let s = wait_next_event [Button_down; Key_pressed] 
     in if s.Graphics.keypressed then
      false else true ;;

(* main function *)
let main : unit = 

	while exec do

	colorMode white black ;
	(* define the tree *)
	let t = Tree( R(120) , N(80. , 0. ) ) in
	let p = root t in
	angle := tf (( (ti !angle) + 1) mod 360) ;
	branch t 10 p ;
	
	Time.wait fps;
	done;;

(** Exec **)

setScreen ;;
main exec ;;
