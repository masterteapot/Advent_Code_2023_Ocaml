open Batteries

type card =
  { index : int
  ; card_number : int
  ; mutable copies : int
  ; num_winners : int
  ; card_points : int
  }

let get_groups input =
  let t_groups = String.split input ~by:" | " in
  let w_str = String.split (fst t_groups) ~by:": " in
  let number = fst w_str |> String.split_on_char ' ' in
  let card_number = List.last number |> int_of_string in
  let c =
    snd t_groups
    |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
    |> List.map int_of_string
  in
  let w =
    snd w_str
    |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
    |> List.map int_of_string
  in
  card_number, w, c
;;

let count_winners card =
  let winners = Tuple3.second card in
  let chosen = Tuple3.third card in
  let num_winners =
    List.filter (fun ch -> List.exists (fun wi -> wi = ch) winners) chosen
  in
  List.length num_winners |> float_of_int
;;

let calculate_card_points num_winners =
  match num_winners with
  | 0. -> 0.
  | 1. -> 1.
  | 2. -> 2.
  | x -> 2. ** (x -. 1.)
;;

let create_cards card =
  let num_winners = count_winners card |> int_of_float in
  let index = Tuple3.first card - 1 in
  let card_number = Tuple3.first card in
  let card_points = calculate_card_points (float_of_int num_winners) |> int_of_float in
  let copies = 1 in
  { index; card_number; num_winners; copies; card_points }
;;

let input = Utilities.read_lines "inputs/4_t.txt" |> Utilities.remove_empty_string
let groups = List.map get_groups input
(* let winners = List.map count_winners groups *)

(* let results = *)
(*   List.map calculate_card_points winners |> List.fold ( +. ) 0. |> int_of_float *)
(* ;; *)

let () = List.iter (fun x -> Printf.printf "\n%d" (Tuple3.first x)) groups
let out_1 = 1
let out_2 = 2

let main () =
  Printf.printf "Day 4 Part 1 --> %d\n" out_1;
  Printf.printf "Day 4 Part 2 --> %d\n" out_2
;;
