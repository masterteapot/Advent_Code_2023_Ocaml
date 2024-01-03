open Base
open Core
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
  let groups = String.split ~on:' ' i in
  assert (List.length groups = 3);
  let rec aux acc = function
    | '(' :: tl -> aux acc tl
    | ')' :: _ -> String.of_list @@ List.rev acc
    | hd :: tl -> aux (hd :: acc) tl
    | [] -> failwith "We should have already exited"
  in
  let color_code =
    aux
      []
      (String.to_list
       @@
       match List.nth groups 2 with
       | Some x -> x
       | None -> failwith "missing string")
  in
  let dir =
    match List.nth groups 0 with
    | Some "R" -> E
    | Some "D" -> S
    | Some "L" -> W
    | Some "U" -> N
    | _ -> failwith "unexpected instruction string"
  in
  let count =
    int_of_string
    @@
    match List.nth groups 1 with
    | Some x -> x
    | None -> failwith "i dunno"
  in
  { dir
  ; count
  ; color_code
  ; c =
      (match List.nth groups 0 with
       | Some x -> x
       | None -> failwith "i dunno 2")
  }
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
  let cur =
    match List.hd acc with
    | Some x -> x
    | None -> failwith "List is empty"
  in
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

(* let measure_trenches trenches = *)
(*   let min_x = List.min (List.map fst trenches) in *)
(*   let max_x = List.max (List.map fst trenches) in *)
(*   let min_y = List.min (List.map snd trenches) in *)
(*   let max_y = List.max (List.map snd trenches) in *)
(* ;; *)

let input =
  read_lines "inputs/18_t.txt" |> remove_empty_string |> List.map ~f:map_instructions
;;

let trenches = List.fold_left input ~init:[ 0, 0 ] ~f:trench_walker
(* let hole_size = measure_trenches trenches *)

(* Part 1 *)
let part_one () =
  let out_1 = 1 in
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
