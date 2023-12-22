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

let parse_inputs input =
  let rec aux input acc =
    match input with
    | [] -> acc
    | hd :: tl ->
      let groups = String.split_on_char ' ' hd in
      let left = List.hd groups |> String.split_on_char '.' |> List.filter (fun x -> x <> "") in
      let right =
        List.at groups 1 |> String.split_on_char ',' |> List.map int_of_string
      in
      aux tl ((left, right) :: acc)
  in
  aux input []
;;

let rec score map key =
    match map, key with
    | _, [] -> 1
    | [], _ -> 0
    | a :: atl, b :: btl when a = b -> score atl btl
    | a :: atl, bls -> score atl bls
;;


let t = score [2; 1; 3; 2; 3] [1; 2; 3] 

let () = print_newline ()
let () = print_newline ()

let () = print_int t

let () = print_newline ()
let () = print_newline ()

(* ???.### 1,1,3 <-- 1 *)
(* .??..??...?##. 1,1,3 <-- 4 *)
(* ?#?#?#?#?#?#?#? 1,3,1,6 <-- 1 *)
(* ????.#...#... 4,1,1  <-- 1 *)
(* ????.######..#####. 1,6,5 <-- 4 *)
(* ?###???????? 3,2,1 <-- 10 *)

(* Total is 21 arrangements *)

(* Part 1 *)
let part_one () =
  let out_1 = 1 in
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
