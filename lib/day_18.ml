open Batteries
open Utilities
open Printf

type direction =
  | N
  | E
  | S
  | W

type instructions =
  { dir : direction
  ; count : int
  ; color_code : string
  ; c : string
  }

let map_instructions i =
  let groups = String.split_on_char ' ' i in
  assert (List.length groups = 3);
  let rec aux acc = function
    | '(' :: tl -> aux acc tl
    | ')' :: tl -> String.of_list @@ List.rev acc
    | hd :: tl -> aux (hd :: acc) tl
    | [] -> failwith "We should have already exited"
  in
  let color_code = aux [] (String.explode @@ List.at groups 2) in
  let dir =
    match List.at groups 0 with
    | "R" -> E
    | "D" -> S
    | "L" -> W
    | "U" -> N
    | _ -> failwith "unexpected instruction string"
  in
  let count = int_of_string (List.at groups 1) in
  { dir; count; color_code; c = List.at groups 0 }
;;

let print_instructions i =
  printf
    "{ dir: %s; count: %d; color_code: %s; c: %s }\n"
    (match i.dir with
     | N -> "N"
     | W -> "W"
     | S -> "S"
     | E -> "E")
    i.count
    i.color_code
    i.c
;;

let trench_walker acc i =
  let cur = List.hd acc in
  let rec aux x y dir count acc =
    if count = 0
    then acc
    else (
      match dir with
      | N -> aux x (pred y) dir (pred count) ((x, pred y) :: acc)
      | E -> aux (succ x) y dir (pred count) ((succ x, y) :: acc)
      | S -> aux x (succ y) dir (pred count) ((x, succ y) :: acc)
      | W -> aux (pred x) y dir (pred count) ((pred x, y) :: acc))
  in
  aux (fst cur) (snd cur) i.dir i.count acc
;;

let measure_trenches trenches acc i =
  let in_trench = List.filter (fun x -> fst x = i) trenches |> List.map snd in
  let min_t = List.min in_trench in
  let max_t = List.max in_trench in
  acc + max_t - min_t + 1
;;

let input =
  read_lines "inputs/18_t.txt" |> remove_empty_string |> List.map map_instructions
;;

let trenches = List.fold_left trench_walker [ 0, 0 ] input
let edge = List.unique ~eq:(fun x y -> fst x = fst y) trenches |> List.map fst
let hole_size = List.fold_left (measure_trenches trenches) 0 edge

(* Part 1 *)
let part_one () =
  let out_1 = hole_size in
  Printf.printf "Day 18 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  Printf.printf "Day 18 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
