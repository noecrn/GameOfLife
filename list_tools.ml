(* #use "topfind" ;;
#require "graphics";; *)
open Graphics ;;
open_graph " 1200x800" ;;

(*length*)
let rec length tab = match tab with
  | [] -> 0
  | e :: l -> 1 + length l ;;

(*nth*)
let nth n list =
  let rec aux = function
      | (_, []) -> invalid_arg "list too short"
      | (0, e::l) -> e
      | (i, e::l) -> aux (i-1, l)
    in
    aux (n, list) ;;

(*prefix*)
let prefix list1 list2 = 
  let rec aux list1 list2 = match list1, list2 with
  | ([], list2) | (list2, []) -> true
  | (e :: list1 , x :: list2) -> if e = x then aux list1 list2 else false
in aux list1 list2 ;;

(*search*)
let search x tab = 
  let rec aux = function
    | [] -> false
    | e :: l when e = x -> true
    | _ :: l -> aux l
in aux tab ;;

(*init_list*)
let rec init_list n x = match n with 
  | 0 -> []
  | _ -> x :: init_list(n-1) x ;;

(*append*)
let rec append list1 list2 = match list1 with
  | [] -> list2
  | e :: l -> e :: (append l list2) ;;

(*put_list*)
let rec put_list v i list = match list with
  | [] -> list
  | e::l -> if (i=0) then
              v::(put_list v (i-1) l)
            else
              e::(put_list v (i-1) l)

(*init_board*)
let init_board (l, c) x = 
  let rec aux j list = match j with 
    | j when j = l -> list
    | j -> aux (j+1) ((init_list c x) :: list) 
in aux 0 [];;

(*get_cell*)
let get_cell (x, y) board = nth y (nth x board) ;;

(*put_cell*)
let put_cell v (x, y) board =
  let rec find_line x board = match board with
    | [] -> invalid_arg "Error"
    | e::l when x = 0 -> e
    | e::l -> find_line (x-1) l
  in let line = find_line x board in
     let rec update i elt lst = match lst with
       | [] -> invalid_arg "Error1"
       | e::l when i = 0 -> elt::l
       | e::l -> e::(update (i-1) elt l)
      in update x (update y v line) board ;;

(* cell_color *)
let cell_color = function
  |0 -> white
  |_ -> black ;;

(* draw_cell *)
let draw_cell (x,y) size color =
  set_color black ;
  draw_rect x y size size ;
  set_color color ;
  fill_rect (x+1) (y+1) (size-2) (size-2) ;;

(* draw_board *)
let draw_board board cellsize =
  let rec draw_column x y list cellsize =
    match list with
    |[] -> ()
    |e::r -> draw_cell (x,y) cellsize (cell_color e) ;
             draw_column x (y+cellsize) r cellsize in
  let rec drawing x y board cellsize =
    match board with
    |[] -> ()
    |e::r -> draw_column x y e cellsize ;
             drawing (x+cellsize) y r cellsize
  in drawing 10 10 board cellsize ;;