open Batteries
open Utilities
open Printf

type operation =
  | Minus
  | Equals

let calculate_hash acc c =
  let x = Char.code c |> ( + ) acc |> ( * ) 17 in
  Int.rem x 256
;;

let parse_instructions s =
  let string_length = String.length s in
  let rec aux counter acc label operator =
    if counter = string_length
    then (
      let focal_length = if String.length acc > 0 then int_of_string acc else 0 in
      label, operator, focal_length)
    else (
      match s.[counter] with
      | '-' -> acc, Minus, 0
      | '=' -> aux (counter + 1) "" acc Equals
      | c -> aux (counter + 1) (acc ^ String.make 1 c) label operator)
  in
  aux 0 "" "" Equals
;;

let print_instructions instruction =
  let operation =
    match Tuple3.second instruction with
    | Minus -> "Minus"
    | Equals -> "Equals"
  in
  printf "%s, %s, %d\n" (Tuple3.first instruction) operation (Tuple3.third instruction)
;;

let print_box b =
  print_newline ();
  List.iter (fun x -> printf "[%s, %d]; " (fst x) (snd x)) b
;;

let operate_boxes boxes instructions =
  let rec equal_box b ls acc =
    match ls with
    | [] -> List.rev (b :: acc)
    | hd :: tl when fst hd = fst b -> List.rev (b :: acc) @ tl
    | hd :: tl -> equal_box b tl (hd :: acc)
  in
  let rec aux = function
    | [] -> boxes
    | (lbl, Minus, fl) :: tl ->
      let hm = String.explode lbl |> List.fold_left calculate_hash 0 in
      let b = boxes.(hm) in
      boxes.(hm) <- List.filter (fun x -> fst x <> lbl) b;
      aux tl
    | (lbl, Equals, fl) :: tl ->
      let hm = String.explode lbl |> List.fold_left calculate_hash 0 in
      let b = boxes.(hm) in
      boxes.(hm) <- equal_box (lbl, fl) b [];
      aux tl
  in
  aux instructions
;;

let calc_focusing_power boxes =
  Array.fold_lefti
    (fun acc i b ->
      acc + List.fold_lefti (fun inner_acc j x -> inner_acc + ((1 + i) * j * snd x)) 0 b)
    0
    boxes
;;

(* Part 1 *)
let part_one () =
  let input =
    read_lines "inputs/15_t.txt"
    |> remove_empty_string
    |> List.hd
    |> String.split_on_char ','
  in
  let output =
    List.map String.explode input |> List.map (List.fold_left calculate_hash 0)
  in
  let out_1 = List.fold_left ( + ) 0 output in
  Printf.printf "Day 16 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let input =
    read_lines "inputs/15.txt"
    |> remove_empty_string
    |> List.hd
    |> String.split_on_char ','
    |> List.map parse_instructions
  in
  let boxes = Array.make 256 [ "", 0 ] in
  let boxes = operate_boxes boxes input in
  let output = calc_focusing_power boxes in
  let out_2 = output in
  Printf.printf "Day 15 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
