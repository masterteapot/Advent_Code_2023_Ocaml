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

let move_stones stones index =
  let rec get_new_index counter =
    if counter = 0
    then 0
    else if stones.(counter - 1).(fst index) = '.'
    then get_new_index (counter - 1)
    else counter
  in
  let col = get_new_index (snd index) in
  if col = snd index then () else stones.(snd index).(fst index) <- '.';
  stones.(col).(fst index) <- 'O'
;;

let stone_walker stones =
  Array.iteri
    (fun i x ->
      if i = 0
      then ()
      else Array.iteri (fun j y -> if y = 'O' then move_stones stones (j, i) else ()) x)
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

let input = read_lines "inputs/14_t.txt" |> remove_empty_string |> List.map String.explode
let stones = make_stone_array input
let () = print_newline ()
let () = print_newline ()
let () = print_simple_stone_array stones
let () = print_newline ()
let () = stone_walker stones
let () = print_newline ()
let () = print_newline ()
let () = print_simple_stone_array stones
let () = print_newline ()

(* Part 1 *)
let part_one () =
  let out_1 = stone_weigher stones in
  Printf.printf "Day 14 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  Printf.printf "Day 14 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
