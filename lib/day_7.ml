open Batteries

type game =
  { hand : int list
  ; mutable hand_strength : int
  ; bet : int
  }

let print_game a_game =
  let hand = List.fold_left (fun acc x -> acc ^ ", " ^ string_of_int x) "" a_game.hand in
  Printf.printf
    "Hand: %s || Hand Strength: %d || Bet: %d\n\n"
    hand
    a_game.hand_strength
    a_game.bet
;;

let make_game jack_value a_str =
  let parts = String.split_on_char ' ' a_str in
  let hand_raw = List.hd parts |> String.explode in
  let bet = int_of_string (List.at parts 1) in
  let rec make_hand hand_raw acc =
    match hand_raw with
    | [] -> List.rev acc
    | x :: tl ->
      let c_value =
        match x with
        | 'A' -> 14
        | 'K' -> 13
        | 'Q' -> 12
        | 'J' -> jack_value
        | 'T' -> 10
        | '9' -> 9
        | '8' -> 8
        | '7' -> 7
        | '6' -> 6
        | '5' -> 5
        | '4' -> 4
        | '3' -> 3
        | '2' -> 2
        | _ -> failwith "not valid card value"
      in
      make_hand tl (c_value :: acc)
  in
  let hand = make_hand hand_raw [] in
  { hand; hand_strength = 0; bet }
;;

let find_hand_strength a_game =
  let sh =
    List.sort (fun x y -> if x > y then 1 else if x = y then 0 else -1) a_game.hand
  in
  let rec count_matches nums acc out =
    match nums with
    | [] -> List.sort (fun x y -> if x > y then -1 else if x = y then 0 else 1) out
    | [ x; y ] when x = y -> count_matches [] 0 ((acc + 1) :: out)
    | x :: y :: tl when x = y -> count_matches (y :: tl) (acc + 1) out
    | _ :: [] -> count_matches [] 1 (1 :: out)
    | _ :: tl -> count_matches tl 1 (acc :: out)
  in
  let vals = count_matches sh 1 [] in
  let hand_strength =
    match vals with
    | [ 5 ] -> 7
    | [ 4; 1 ] -> 6
    | [ 3; 2 ] -> 5
    | [ 3; 1; 1 ] -> 4
    | [ 2; 2; 1 ] -> 3
    | [ 2; 1; 1; 1 ] -> 2
    | [ 1; 1; 1; 1; 1 ] -> 1
    | _ -> failwith "not a valid hand"
  in
  a_game.hand_strength <- hand_strength
;;

let find_hand_strength_v2 a_game =
  let pre =
    List.sort (fun x y -> if x > y then -1 else if x = y then 0 else 1) a_game.hand
  in
  let joke_index = List.find_index (fun x -> x = 0) pre in
  let sh, jokes =
    match joke_index with
    | Some i -> List.takedrop i pre
    | None -> pre, []
  in
  let j_len = List.length jokes in
  let rec count_matches nums acc out =
    match nums with
    | [] -> List.sort (fun x y -> if x > y then -1 else if x = y then 0 else 1) out
    | [ x; y ] when x = y -> count_matches [] 0 ((acc + 1) :: out)
    | x :: y :: tl when x = y -> count_matches (y :: tl) (acc + 1) out
    | _ :: [] -> count_matches [] 1 (1 :: out)
    | _ :: tl -> count_matches tl 1 (acc :: out)
  in
  let pre_vals = count_matches sh 1 [] in
  let vals =
    match j_len with
    | 0 -> pre_vals
    | 5 -> [ 5 ]
    | x ->
      let new_max = List.hd pre_vals + x in
      new_max :: List.tl pre_vals
  in
  let hand_strength =
    match vals with
    | [ 5 ] -> 7
    | [ 4; 1 ] -> 6
    | [ 3; 2 ] -> 5
    | [ 3; 1; 1 ] -> 4
    | [ 2; 2; 1 ] -> 3
    | [ 2; 1; 1; 1 ] -> 2
    | [ 1; 1; 1; 1; 1 ] -> 1
    | _ -> failwith "not a valid hand"
  in
  a_game.hand_strength <- hand_strength
;;

let compare_hands x y =
  let rec match_singles left right =
    match left, right with
    | x :: _, y :: _ when x > y -> 1
    | x :: _, y :: _ when x < y -> -1
    | _ :: tl, _ :: yl -> match_singles tl yl
    | [], [] -> 0
    | _, _ -> failwith "different length lists"
  in
  if x.hand_strength > y.hand_strength
  then 1
  else if x.hand_strength < y.hand_strength
  then -1
  else match_singles x.hand y.hand
;;

(* Part 1 *)
let input = Utilities.read_lines "inputs/7.txt" |> Utilities.remove_empty_string
let hands = List.map (make_game 11) input
let () = List.iter find_hand_strength hands
let sorted_hands = List.sort compare_hands hands
let score = List.fold_lefti (fun acc i hand -> acc + ((i + 1) * hand.bet)) 0 sorted_hands

(* Part 2 *)
let hands_v2 = List.map (make_game 0) input
let () = List.iter find_hand_strength_v2 hands_v2
let sorted_hands_v2 = List.sort compare_hands hands_v2

let score_v2 =
  List.fold_lefti (fun acc i hand -> acc + ((i + 1) * hand.bet)) 0 sorted_hands_v2
;;

let main () =
  let out_1 = score in
  let out_2 = score_v2 in
  Printf.printf "Day 4 Part 1 --> %d\n" out_1;
  Printf.printf "Day 4 Part 2 --> %d\n" out_2
;;
