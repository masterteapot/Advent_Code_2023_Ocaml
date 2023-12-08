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

let create_cards groups =
  let num_winners = count_winners groups |> int_of_float in
  let index = Tuple3.first groups - 1 in
  let card_number = Tuple3.first groups in
  let card_points = calculate_card_points (float_of_int num_winners) |> int_of_float in
  let copies = 1 in
  { index; card_number; num_winners; copies; card_points }
;;

let print_card card =
  Printf.printf
    "\ncn: %d | nw: %d | cp: %d | copies: %d"
    card.card_number
    card.num_winners
    card.card_points
    card.copies
;;

let update_copies all_cards card =
  match card.num_winners with
  | 0 -> ()
  | num_winners ->
    let indices = List.range 1 `To num_winners in
    let update_card rel_index =
      let c = List.at all_cards (card.index + rel_index) in
      c.copies <- c.copies + card.copies
    in
    List.iter update_card indices
;;

let input = Utilities.read_lines "inputs/4.txt" |> Utilities.remove_empty_string
let groups = List.map get_groups input
let cards = List.map create_cards groups
let () = List.iter (update_copies cards) cards
let out_1 = List.fold_left (fun acc c -> acc + c.card_points) 0 cards
let out_2 = List.fold_left (fun acc c -> acc + c.copies) 0 cards

let main () =
  Printf.printf "Day 4 Part 1 --> %d\n" out_1;
  Printf.printf "Day 4 Part 2 --> %d\n" out_2
;;
