#use "gameOfLife.ml" ;;
open Graphics ;;

let write filename list =
  let oc = open_out filename in
  let rec aux = function
  [] -> close_out oc
  | e :: l -> Printf . fprintf oc "%s " e ; aux l
  in aux list ;;

let load name =
  let ic = open_in name in
    let try_read () = try Some ( input_line ic ) with 
      End_of_file -> None in
    let rec loop () = match try_read () with
      | Some s -> s ::( loop () )
      | None -> close_in ic ; []
    in loop () ;;

(* load_board *)
let load_board name =
  let ic = open_in name in
    let try_read () = 
      try Some (input_char ic) with End_of_file -> None in
    let rec line() = match try_read () with
      | Some s when s = '1' -> 1::(line ())
      | Some s when s = '0' -> 0::(line ())
      | Some s when s = ' ' -> line ()
      | Some s when s = '\n' -> []
      | Some s -> line ()
      | None -> close_in ic; []
in let rec loop () = match line () with
      | [] -> close_in ic ; []
      | s -> s ::(loop ())
    in loop () ;;

(* save_board *)
let save_board filename list =
  let oc = open_out filename in
  let rec aux = function
  | [] -> close_out oc
  | e :: l -> Printf . fprintf oc "%s " (string_of_int e) ; aux l
in aux list ;;

(* init_pattern *)
let init_pattern pattern size =
  let board = init_board (size,size) 0 in
  let rec rec_pattern pattern board =
    match pattern with
    |[] -> board
    |e::r -> rec_pattern r (put_cell 1 e board)
  in rec_pattern pattern board ;;

(* new_game_pattern *)
let new_game_pattern board size nb = game board size nb ;;

(* remaining *)
let rec remaining_column list =
  match list with
  |[] -> false
  |e::r when e=0 -> remaining_column r
  |_ -> true ;;

let rec remaining board =
  match board with
  |[] -> false
  |e::r when (not (remaining_column e)) -> remaining r
  |_ -> true ;;

(* new_game_survival *)
let rec new_game_survival size nb_cells =
  let rec next_gen board =
    if remaining (next_generation board size)
    then
      next_gen (next_generation board size)
    else
      board
  in next_gen (new_board size nb_cells) ;;

(* new_game_pattern_survival *)
let rec new_game_pattern_survival board size =
  if remaining (next_generation board size)
  then
    new_game_pattern_survival (next_generation board size) size
  else
    board ;;

(* la vie sans redessine le plateau a chaque generation *)
let rec game board size n =
  match n with
  |0 -> draw_board board 10
  |_ when n < 0 -> invalid_arg "n must be a natural"
  |_ -> game (next_generation board size) size (n-1) ;;

let new_game size nb_cell n =
  game (new_board size nb_cell) size n ;;