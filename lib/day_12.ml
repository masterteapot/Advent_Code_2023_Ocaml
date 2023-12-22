open Batteries
open Utilities

let print_diagram map =
  let field, digits =
    match map with
    | field, digits -> field, digits
  in
  print_string "| [ ";
  List.iter (fun x -> Printf.printf "%c " x) field;
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
      let left = List.hd groups |> String.explode in
      let right =
        List.at groups 1 |> String.split_on_char ',' |> List.map int_of_string
      in
      aux tl ((left, right) :: acc)
  in
  aux input []
;;

(* TODO need to remove 0s and need to add acc when not 0s into if/else statement *)
let walker (map, key) =
  let key_length = List.length key in
  let rec aux counter acc = function
    | '#' :: tl -> aux (succ counter) acc tl
    | '.' :: tl when counter > 0 -> aux 0 (counter :: acc) tl
    | '.' :: tl -> aux 0 acc tl
    | '?' :: tl when counter > 0 -> aux (succ counter) acc tl + aux 0 (counter :: acc) tl
    | '?' :: tl -> aux (succ counter) acc tl + aux 0 acc tl
    | [] ->
      let acc = if counter > 0 then counter :: acc else acc in
      if List.length acc <> key_length
      then 0
      else (
        let score = if (List.rev acc) = key then 1 else 0 in
        (* print_endline "we scored the group :"; *)
        (* List.iter print_int (List.rev (counter :: acc)); *)
        (* Printf.printf " as: %d\n\n" score; *)
        score)
    | _ -> failwith "unexpected character in map"
  in
  aux 0 [] map
;;

(* ???.### 1,1,3 <-- 1 *)
(* .??..??...?##. 1,1,3 <-- 4 *)
(* ?#?#?#?#?#?#?#? 1,3,1,6 <-- 1 *)
(* ????.#...#... 4,1,1  <-- 1 *)
(* ????.######..#####. 1,6,5 <-- 4 *)
(* ?###???????? 3,2,1 <-- 10 *)

(* Total is 21 arrangements *)

let () = print_newline ()
let input = read_lines "./inputs/12_t.txt" |> remove_empty_string |> parse_inputs

let counted = List.map walker input
let res_1 = (List.fold_left ( + ) 0 counted)
let t = List.at input 5
let tv = walker t

(* let () = List.iter (fun x -> Printf.printf " %d " x) counted *)
let () = print_newline ()
let () = print_diagram t
let () = print_newline ()
let () = print_newline ()

(* Part 1 *)
let part_one () =
  let out_1 = res_1 in
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
