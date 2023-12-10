open Batteries

let parse_ints all_chars =
  let rec aux all_chars inner acc =
    match all_chars with
    | hd :: tl ->
            if Char.is_digit hd || hd = '-'
      then aux tl (inner ^ String.make 1 hd) acc
      else if String.length inner > 0
      then aux tl "" (int_of_string inner :: acc)
      else aux tl "" acc
    | [] -> List.rev (int_of_string inner :: acc)
  in
  aux all_chars "" []
;;

let find_diffs nums =
  let rec aux nums acc =
    match nums with
    | x :: y :: tl -> aux (y :: tl) ((y - x) :: acc)
    | _ :: [] -> List.rev acc
    | [] -> List.rev acc
  in
  aux nums []
;;

let run_calcs nums =
  let rec aux nums acc =
    let calcs = find_diffs nums in
    if List.for_all (fun x -> x = 0) calcs then calcs :: acc else aux calcs (calcs :: acc)
  in
  aux nums [ nums ]
;;

let print_lld a_list =
    print_newline ();
  List.iter
    (fun x ->
      print_newline ();
      List.iter (fun y -> Printf.printf "%d, " y) x;
      print_newline ())
    a_list;
    print_newline ()
;;

let calc_end_value calc_list = List.fold_left (fun acc x -> acc + List.last x) 0 calc_list

let calc_start_value calc_list = 
    let rec aux calc_list =
        match calc_list with
        | hd :: md :: tl -> 
            let cur_start = List.hd md in
            let last_start = List.hd hd in
            let new_head = cur_start - last_start in
            aux ((new_head :: md) :: tl)
        | hd :: [] -> List.hd hd
        | [] -> failwith "List is too short"
    in
    aux calc_list
;;

  let input =
    Utilities.read_lines "inputs/9.txt"
    |> Utilities.remove_empty_string
    |> List.map String.explode
    |> List.map (fun x -> parse_ints x)
    |> List.map run_calcs
(* Part 1 *)
let part_one () =
  let out_1 = input |> List.map calc_end_value |> List.fold_left ( + ) 0 in
  Printf.printf "Day 9 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = input |> List.map calc_start_value  |> List.fold_left ( + ) 0 in
  Printf.printf "Day 9 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
