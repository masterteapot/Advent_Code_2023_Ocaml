open Batteries

let print_htv ht k =
  let v = Hashtbl.find ht k in
  Printf.printf "(%s, %S);\n" (fst v) (snd v)
;;

let print_ht k v = Printf.printf "%s => (%s, %S);\n" k (fst v) (snd v)

let make_ht all_strings =
  let rec get_lr left right acc to_nodes =
    match to_nodes with
    | [] -> left, right
    | ',' :: tl -> get_lr acc right "" tl
    | ')' :: tl -> get_lr left acc "" tl
    | x :: tl when Char.is_letter x || Char.is_digit x ->
      get_lr left right (acc ^ String.make 1 x) tl
    | _ :: tl -> get_lr left right "" tl
  in
  let update_ht hd ht =
    let parts = String.split_on_string ~by:" = " hd in
    let name = List.hd parts in
    let lrs = List.at parts 1 |> String.explode in
    let left_name, right_name = get_lr "" "" "" lrs in
    Hashtbl.add ht name (left_name, right_name)
  in
  let rec aux all_strings ht =
    match all_strings with
    | [] -> ht
    | hd :: tl ->
      update_ht hd ht;
      aux tl ht
  in
  aux all_strings (Hashtbl.create (List.length all_strings))
;;

let trace_path start_name finish_name ht directions =
  let rec aux in_directions cur_name counter =
    match in_directions with
    | [] -> aux directions cur_name counter
    | _ when cur_name = finish_name -> counter
    | d :: tl ->
      let lrs = Hashtbl.find ht cur_name in
      let new_name =
        match d with
        | 'L' -> fst lrs
        | 'R' -> snd lrs
        | _ -> failwith "unmatched directions"
      in
      aux tl new_name (counter + 1)
  in
  aux directions start_name 0
;;

let array_of_list xs =
  match xs with
  | [] -> [||]
  | default :: _ ->
    let arr = Array.make (List.length xs) default in
    List.iteri (Array.set arr) xs;
    arr
;;

let extra_reduced all_lists =
  let all_values =
    List.flatten all_lists
    |> List.sort (fun x y ->
      if fst x > fst y || snd x > snd y then -1 else if x = y then 0 else 1)
  in
  let rec aux all_values acc =
    match all_values with
    | [] -> List.rev acc
    | hd :: [] -> aux [] (hd :: acc)
    | hd :: md :: tl ->
      if fst hd = fst md then aux (hd :: tl) acc else aux (md :: tl) (hd :: acc)
  in
  aux all_values []
;;

let factor_groups n =
  let rec aux d n =
    if n = 1
    then []
    else if n mod d = 0
    then (
      match aux d (n / d) with
      | (h, n) :: t when h = d -> (h, n + 1) :: t
      | l -> (d, 1) :: l)
    else aux (d + 1) n
  in
  aux 2 n
;;

let trace_path_v2 finish_char ht directions start_group =
  let update_group direction name =
    let lrs = Hashtbl.find ht name in
    match direction with
    | 'L' -> fst lrs
    | 'R' -> snd lrs
    | _ -> failwith "unmatched directions"
  in
  let rec aux in_directions counter name =
    match in_directions with
    | [] -> aux directions counter name
    | _ when name.[2] = finish_char -> counter
    | d :: tl ->
      let new_name = update_group d name in
      aux tl (counter + 1) new_name
  in
  List.map (aux directions 0) start_group
;;

let rec pow x y =
    if y=0 then 1
    else x * pow x (y - 1)
;;

(* Part 1 *)
let part_one () =
  let input = Utilities.read_lines "inputs/8.txt" |> Utilities.remove_empty_string in
  let instructions = String.explode (List.hd input) in
  let ht = make_ht (List.tl input) in
  let start_name = "AAA" in
  let finish_name = "ZZZ" in
  let out_1 = trace_path start_name finish_name ht instructions in
  Printf.printf "Day 4 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () = 
    let input_2 = Utilities.read_lines "inputs/8.txt" |> Utilities.remove_empty_string in
    let instruct_2 = String.explode (List.hd input_2) in
    let ht_2 = make_ht (List.tl input_2) in
    let names = Hashtbl.to_list ht_2 |> List.map (fun x -> fst x) in
    let start_char = 'A' in
    let finish_char = 'Z' in
    let start_group = List.find_all (fun x -> x.[2] = start_char) names in
    let maybe_2 = trace_path_v2 finish_char ht_2 instruct_2 start_group in
    let maybe_3 = maybe_2 |> List.map factor_groups |> extra_reduced in
    let out_2 = List.fold_left (fun acc x -> acc * (pow (fst x) (snd x))) 1 maybe_3 in
    Printf.printf "Day 4 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
