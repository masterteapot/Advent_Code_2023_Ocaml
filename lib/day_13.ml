open Batteries
(* open Utilities *)

let explode_puzzles puzzle =
  let rec aux h_puzzle shrinking_puzzle vert_chars acc =
    match h_puzzle with
    | [] when List.length (List.hd shrinking_puzzle) = 0 ->
      vert_chars :: acc |> List.map List.rev |> List.map String.of_list |> List.rev
    | [] -> aux (List.rev shrinking_puzzle) [] [] (vert_chars :: acc)
    | chars :: otl ->
      (match chars with
       | [] -> failwith "this shouldn't happen"
       | c :: tl -> aux otl (tl :: shrinking_puzzle) (c :: vert_chars) acc)
  in
  let v_input = List.map String.explode puzzle in
  let v_puzzle = aux v_input [] [] [] in
  puzzle, v_puzzle
;;

let switch_char index a_str b_str =
  let sub = String.get b_str index |> String.make 1 in
  String.splice a_str index 1 sub
;;

let find_smudge a_str b_str =
  let ln = String.length a_str in
  let rec aux loop_counter mismatch_counter mismatch_index =
    if loop_counter = ln && mismatch_counter = 1
    then mismatch_index
    else if loop_counter = ln
    then -1
    else if a_str.[loop_counter] <> b_str.[loop_counter]
    then aux (loop_counter + 1) (mismatch_counter + 1) loop_counter
    else aux (loop_counter + 1) mismatch_counter mismatch_index
  in
  let mismatch_score = aux 0 0 0 in
  let result =
    match mismatch_score with
    | -1 -> false, a_str, b_str
    | n -> true, switch_char n a_str b_str, b_str
  in
  result
;;

(* Instead of trying to find two matches next to each other, I need to walk out and check opposing ones when finding a natural match as well!!! *)
let find_solution_p2 input =
  let rec aux input acc =
    match input with
    | hd :: md :: tl when Tuple3.first (find_smudge hd md) ->
      let _, new_hd, new_md = find_smudge hd md in
      print_endline "";
      print_endline "we found a result";
      print_endline (new_hd ^ " | " ^ new_md);
      let a = new_hd :: acc in
      let b = new_md :: tl in
      let a_len = List.length a in
      let b_len = List.length b in
      if a = b || (a_len > b_len && List.take b_len a = b) || a = List.take a_len b
      then a_len
      else aux (md :: tl) (hd :: acc)
    | hd :: tl -> aux tl (hd :: acc)
    | [] -> 0
  in
  aux input []
;;

let count_points_p2 reflections =
  let rows = find_solution_p2 (fst reflections) in
  let cols = find_solution_p2 (snd reflections) in
  if rows > 0 && cols > 0
  then failwith "What is going on? More than 1 solutions?"
  else if rows > 0
  then rows * 100
  else if cols > 0
  then cols
  else failwith "What is going on? No answers?"
;;

let find_solution input =
  let rec aux input acc =
    match input with
    | hd :: md :: tl when hd = md ->
      let a = hd :: acc in
      let b = md :: tl in
      let a_len = List.length a in
      let b_len = List.length b in
      if a = b || (a_len > b_len && List.take b_len a = b) || a = List.take a_len b
      then a_len
      else aux (md :: tl) (hd :: acc)
    | hd :: tl -> aux tl (hd :: acc)
    | [] -> 0
  in
  aux input []
;;

let count_points reflections =
  let rows = find_solution (fst reflections) in
  let cols = find_solution (snd reflections) in
  if rows > 0 && cols > 0
  then failwith "What is going on? More than 1 solutions?"
  else if rows > 0
  then rows * 100
  else if cols > 0
  then cols
  else failwith "What is going on? No answers?"
;;

let group_input input =
  let rec aux input smaller_acc acc =
    match input with
    | [] -> acc
    | hd :: "" :: tl -> aux tl [] (List.rev (hd :: smaller_acc) :: acc)
    | hd :: tl -> aux tl (hd :: smaller_acc) acc
  in
  aux input [] []
;;

let input_p2 = Utilities.read_lines "inputs/13_t.txt"
let grouped_input_p2 = group_input input_p2
let exploded_p2 = List.map explode_puzzles grouped_input_p2
let points_p2 = List.map count_points_p2 exploded_p2 |> List.fold_left ( + ) 0

(* Part 1 *)
let part_one () =
  let input = Utilities.read_lines "inputs/13.txt" in
  let grouped_input = group_input input in
  let exploded = List.map explode_puzzles grouped_input in
  let points = List.map count_points exploded |> List.fold_left ( + ) 0 in
  let out_1 = points in
  Printf.printf "Day 13 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 0 in
  Printf.printf "Day 13 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
