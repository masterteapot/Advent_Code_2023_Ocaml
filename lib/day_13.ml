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

(* walk till match, check smudges adjacent *)
let reg_match_smudge_finish input =
  let rec smudgy_finish a_list b_list counter =
    match a_list, b_list with
    | [], [] when counter = 1 -> true
    | [], [] -> false
    | _ when counter > 1 -> false
    | ad :: al, bd :: bl when ad = bd -> smudgy_finish al bl counter
    | ad :: al, bd :: bl when Tuple3.first (find_smudge ad bd) ->
      smudgy_finish al bl (counter + 1)
    | ad :: al, bd :: bl when not @@ Tuple3.first (find_smudge ad bd) -> false
    | _ -> failwith "Unexpected case of matching smudgy lists"
  in
  let rec aux input acc =
    match input with
    | hd :: md :: tl when hd = md ->
      let a = hd :: acc in
      let b = md :: tl in
      let a_len = List.length a in
      let b_len = List.length b in
      let a_crop, b_crop =
        if a_len = b_len
        then a, b
        else if a_len > b_len
        then List.take b_len a, b
        else a, List.take a_len b
      in
      if smudgy_finish a_crop b_crop 0 then a_len else aux (md :: tl) (hd :: acc)
    | hd :: tl -> aux tl (hd :: acc)
    | [] -> 0
  in
  aux input []
;;

(* walk matching smudges until a match *)
let smudge_match_reg_finish input =
  let rec aux input acc =
    match input with
    | hd :: md :: tl when Tuple3.first (find_smudge hd md) ->
      let _, new_hd, new_md = find_smudge hd md in
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

let count_points_p2 reflections =
  let rows_smudge_start = smudge_match_reg_finish (fst reflections) in
  let rows_smudge_finish = reg_match_smudge_finish (fst reflections) in
  let cols_smudge_start = smudge_match_reg_finish (snd reflections) in
  let cols_smudge_finish = reg_match_smudge_finish (snd reflections) in
  let rows =
    if rows_smudge_start > 0
    then rows_smudge_start
    else if rows_smudge_finish > 0
    then rows_smudge_finish
    else 0
  in
  let cols =
    if cols_smudge_start > 0
    then cols_smudge_start
    else if cols_smudge_finish > 0
    then cols_smudge_finish
    else 0
  in
  if rows > 0 && cols > 0
  then failwith "What is going on? More than 1 solutions?"
  else if rows > 0
  then rows * 100
  else if cols > 0
  then cols
  else failwith "What is going on? No answers?"
;;

let () = print_newline ()

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
  let input_p2 = Utilities.read_lines "inputs/13.txt" in
  let grouped_input_p2 = group_input input_p2 in
  let exploded_p2 = List.map explode_puzzles grouped_input_p2 in
  let points_p2 = List.map count_points_p2 exploded_p2 |> List.fold_left ( + ) 0 in
  let out_2 = points_p2 in
  Printf.printf "Day 13 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
