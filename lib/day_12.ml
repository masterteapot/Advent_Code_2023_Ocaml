open Batteries
open Utilities

let print_diagram map =
  let field, digits =
    match map with
    | field, digits -> field, digits
  in
  print_string "| [ ";
  List.iter
    (fun x ->
      print_string "[ ";
      List.iter (fun y -> Printf.printf "%c " y) x;
      print_string "] ")
    field;
  print_string "] ";
  print_string " ||  ( ";
  List.iter (fun x -> Printf.printf "%d " x) digits;
  print_string ")"
;;

let break_maps map =
  let rec aux map ls acc =
    match map with
    | [] ->
      let new_acc = if ls = [] then acc else List.rev ls :: acc in
      List.rev new_acc
    | '.' :: tl ->
      let new_acc = if ls = [] then acc else List.rev ls :: acc in
      aux tl [] new_acc
    | c :: tl -> aux tl (c :: ls) acc
  in
  aux map [] []
;;

let parse_inputs input =
  let rec aux input acc =
    match input with
    | [] -> acc
    | hd :: tl ->
      let groups = String.split_on_char ' ' hd in
      let left = List.hd groups |> String.explode |> break_maps in
      let right =
        List.at groups 1 |> String.split_on_char ',' |> List.map int_of_string
      in
      aux tl ((left, right) :: acc)
  in
  aux input []
;;

let split_on_next fields =
  print_llc fields;
  let rec aux fields =
    match fields with
    | [] -> false, []
    | hd :: otl ->
      (match hd with
       | [] -> aux otl
       | '?' :: itl -> true, itl :: otl
       | _ :: itl -> aux (itl :: otl))
  in
  aux fields
;;

let count_solutions map =
  let field, digits =
    match map with
    | f, d -> f, d
  in
  (* let num_digits = List.length digits in *)
  let rec aux inner_digits inner_field outer_field acc =
    match inner_digits with
    | [] ->
      let more_splits = split_on_next outer_field in
      print_newline ();
      if fst more_splits
      then aux digits (snd more_splits) (snd more_splits) (acc + 1)
      else acc + 1
    | n :: ntl ->
      (match inner_field with
       | [] -> acc
       | f :: ftl when List.length f = n -> aux ntl ftl outer_field acc
       | f :: ftl when List.length f < n -> aux (n :: ntl) ftl outer_field acc
       | f :: ftl when List.length f > n ->
         let split_field = List.drop n f in
         aux ntl (split_field :: ftl) outer_field acc
       | _ -> failwith "how can this be possible?")
  in
  aux digits field field 0
;;

let input = Utilities.read_lines "inputs/12_t.txt" |> Utilities.remove_empty_string
let clean_out = parse_inputs input
let t = List.at clean_out 5
let solution = count_solutions t
let () = print_newline ()
let () = print_newline ()
let () = List.iter print_endline input
let () = print_newline ()
let () = print_diagram t

(* ???.### 1,1,3 <-- 1 *)
(* .??..??...?##. 1,1,3 <-- 4 *)
(* ?#?#?#?#?#?#?#? 1,3,1,6 <-- 1 *)
(* ????.#...#... 4,1,1  <-- 1 *)
(* ????.######..#####. 1,6,5 <-- 4 *)
(* ?###???????? 3,2,1 <-- 10 *)

(* Total is 21 arrangements *)

(* Part 1 *)
let part_one () =
  let out_1 = solution in
  Printf.printf "Day 12 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  Printf.printf "Day 12 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;

