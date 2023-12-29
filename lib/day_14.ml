open Batteries
open Utilities

let make_stone_array input =
  List.map List.to_seq input |> List.map Array.of_seq |> List.to_seq |> Array.of_seq
;;

let print_simple_stone_array pipe_array =
  Array.iter
    (fun x ->
      print_newline ();
      Array.iter (fun y -> Printf.printf " %c " y) x)
    pipe_array;
  print_newline ()
;;

let move_stones_north stones index =
  let rec get_new_index y_val =
    if y_val = 0
    then 0
    else if stones.(y_val - 1).(fst index) = '.'
    then get_new_index (y_val - 1)
    else y_val
  in
  let y = get_new_index (snd index) in
  if y = snd index then () else stones.(snd index).(fst index) <- '.';
  stones.(y).(fst index) <- 'O'
;;

let move_stones_east stones index max_x =
  let rec get_new_index x_val =
    if x_val = max_x - 1
    then x_val
    else if stones.(snd index).(x_val + 1) = '.'
    then get_new_index (x_val + 1)
    else x_val
  in
  let x = get_new_index (fst index) in
  if x = fst index then () else stones.(snd index).(fst index) <- '.';
  stones.(snd index).(x) <- 'O'
;;

let move_stones_south stones index max_y =
  let rec get_new_index y_val =
    if y_val = max_y - 1
    then y_val
    else if stones.(y_val + 1).(fst index) = '.'
    then get_new_index (y_val + 1)
    else y_val
  in
  let y = get_new_index (snd index) in
  if y = snd index then () else stones.(snd index).(fst index) <- '.';
  stones.(y).(fst index) <- 'O'
;;

let move_stones_west stones index =
  let rec get_new_index x_val =
    if x_val = 0
    then x_val
    else if stones.(snd index).(x_val - 1) = '.'
    then get_new_index (x_val - 1)
    else x_val
  in
  let x = get_new_index (fst index) in
  if x = fst index then () else stones.(snd index).(fst index) <- '.';
  stones.(snd index).(x) <- 'O'
;;

let stone_walker stones =
  Array.iteri
    (fun i x ->
      if i = 0
      then ()
      else
        Array.iteri (fun j y -> if y = 'O' then move_stones_north stones (j, i) else ()) x)
    stones
;;

let rev_iteri f a =
  let max_i = Array.length a - 1 in
  for i = max_i downto 0 do
    f i a.(i)
  done
;;

let circle_walker stones =
  let max_x = Array.length stones.(0) in
  let max_y = Array.length stones in
  Array.iteri
    (fun i x ->
      Array.iteri (fun j y -> if y = 'O' then move_stones_north stones (j, i) else ()) x)
    stones;
  Array.iteri
    (fun i x ->
      Array.iteri (fun j y -> if y = 'O' then move_stones_west stones (j, i) else ()) x)
    stones;
  rev_iteri
    (fun i x ->
      Array.iteri
        (fun j y -> if y = 'O' then move_stones_south stones (j, i) max_y else ())
        x)
    stones;
  Array.iteri
    (fun i x ->
      rev_iteri
        (fun j y -> if y = 'O' then move_stones_east stones (j, i) max_x else ())
        x)
    stones
;;

let stone_weigher stones =
  let stones_len = Array.length stones in
  Array.fold_lefti
    (fun acc i x ->
      acc
      + Array.fold_left
          (fun in_acc y -> if y = 'O' then in_acc + (stones_len - i) else in_acc)
          0
          x)
    0
    stones
;;

let washing_machine stones =
  let stones = Array.copy stones in
  let rec aux cycle match_counter in_list out_list =
    if match_counter = 30
    then cycle, List.rev out_list
    else (
      circle_walker stones;
      let score = stone_weigher stones in
      let cycle = cycle + 1 in
      let new_list = score :: in_list in
      let out_list =
        if List.length new_list > List.length out_list then new_list else out_list
      in
      let match_counter =
        if new_list = out_list then match_counter + 1 else match_counter
      in
      let in_list = if new_list = out_list then [] else new_list in
      aux cycle match_counter in_list out_list)
  in
  aux 1 0 [] []
;;

let () = print_newline ()

(* Part 1 *)
let part_one () =
  let input =
    read_lines "inputs/14.txt" |> remove_empty_string |> List.map String.explode
  in
  let stones = make_stone_array input in
  let () = stone_walker stones in
  let out_1 = stone_weigher stones in
  Printf.printf "Day 14 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let input =
    read_lines "inputs/14.txt" |> remove_empty_string |> List.map String.explode
  in
  let stones = make_stone_array input in
  let results = washing_machine stones in
  let num_needed = 1000000000 - fst results in
  let remainder = Int.rem num_needed (List.length (snd results)) in
  let score = List.at (snd results) remainder in
  let out_2 = score in
  Printf.printf "Day 14 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
