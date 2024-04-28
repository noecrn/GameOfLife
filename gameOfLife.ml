#use "list_tools.ml" ;;
(* open Graphics ;; *)

(* rules0 *)
let rec rules0 cell near = match (cell,near) with
  | (0,3) -> 1
  | (1,near) when near = 2 || near = 3 -> 1
  | _ -> 0 ;;

(* count_neighbours *)
let neighbours x list =
  if x < 0 then invalid_arg "incorrect indexes"
  else
    let rec neighbours x list =
      match (x+1),list with
     |0,e::r when e = 1 -> 1
     |n,e::r when (e=1 && n=1 || e=1 && n=2)-> 1 +(neighbours (x-1) r)
     |0,e::r -> 0
     |1,e::r |2,e::r -> 0 + (neighbours (x-1) r)
     |_,e::r -> neighbours (x-1) r
     |0,[] -> 0
     |_,[] -> invalid_arg "incorrect indexes"
    in neighbours x list ;;

let count_neighbours (x,y) board =
  if x < 0 || y < 0 then invalid_arg "incorrect indexes"
  else
    let rec rec_count x board =
      match (x+1),board with
      |0,e::r -> neighbours y e
      |n,e::r when (n = 1 || n = 2) ->
        (neighbours y e) + rec_count (x-1) r
      |_,e::r -> rec_count (x-1) r
      |0,[] -> 0
      |_ -> invalid_arg "incorrect indexes"
    in
    if (get_cell (x,y) board) <> 0 then ((rec_count x board)-1)
    else
      rec_count x board ;;

(* seed_life *)
let rec seed_life board size nb_cell = match nb_cell with
  | 0 -> board
  | nb_cell when nb_cell < 0 -> invalid_arg "negative value"
  | _ -> seed_life (put_cell 1 (Random.int(size), Random.int(size)) board) size (nb_cell - 1) ;;

(* new_board *)
let new_board size nb_cell = seed_life (init_board (size, size) 0 ) size nb_cell ;;

(* next_generation *)
let next_generation board size =
  clear_graph () ;
  let next_board = init_board (size,size) 0 in
  let rec cell_rules (x,y) next_board =
    let max = (size-1) in
    match x,y with
    |_ when x = max && y = max -> let status = rules0 (get_cell (x,y) board) (count_neighbours (x,y) board) in
                          put_cell status (x,y) next_board
    |_ when y = max -> let status = rules0 (get_cell (x,y) board) (count_neighbours (x,y) board) in
                       cell_rules ((x+1),0) (put_cell status (x,y) next_board)
    |_ -> let status = rules0 (get_cell (x,y) board) (count_neighbours (x,y) board) in
          cell_rules (x,(y+1)) (put_cell status (x,y) next_board)
  in cell_rules (0,0) next_board ;;

(* game *)
let game board size n =
  let rec aux board size1 n1 = match n1 with
  | 0 -> ()
  | _ -> begin
    draw_board board 10 ;
    aux (next_generation board size ) size1 (n1-1) ;
  end
  in aux board size n ;;

let cell_size = 10 ;;

(* new_game *)
let new_game size nb_cell n = game (new_board size nb_cell) size n ;;