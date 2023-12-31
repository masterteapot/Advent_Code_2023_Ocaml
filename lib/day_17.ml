open Batteries
open Utilities
open Printf

type dir =
  | N
  | E
  | S
  | W

type movement =
  | Left
  | Right
  | Straight

let array_of_list ls =
  match ls with
  | [] -> [||]
  | default :: _ ->
    let arr = Array.make (List.length ls) default in
    List.iteri (Array.set arr) ls;
    arr
;;

let city_map input aai = List.iteri (fun i x -> Array.set aai i (array_of_list x)) input

let get_directions x y d m =
  match d, m with
  | N, Left -> pred x, y, W
  | E, Left -> x, pred y, N
  | S, Left -> succ x, y, E
  | W, Left -> x, succ y, S
  | N, Right -> succ x, y, E
  | E, Right -> x, succ y, S
  | S, Right -> pred x, y, W
  | W, Right -> x, pred y, N
  | N, Straight -> x, pred y, N
  | E, Straight -> succ x, y, E
  | S, Straight -> x, succ y, S
  | W, Straight -> pred x, y, W
;;

let walker aai =
  let max_y = pred @@ Array.length aai in
  let max_x = pred @@ Array.length aai.(0) in
  let best_route = ref 125 in
  let rec aux x y direction hl sc lc rc =
    if x < 0 || x > max_x || y < 0 || y > max_y
    then ()
    else (
      match x, y with
      | x, y when x = max_x && y = max_y ->
        if hl + aai.(y).(x) < !best_route then best_route := hl + aai.(y).(x) else ()
      | _ when hl >= !best_route -> ()
      | x, y ->
        if sc < 4
        then (
          let new_x, new_y, new_dir = get_directions x y direction Straight in
          aux new_x new_y new_dir (hl + aai.(y).(x)) (sc + 1) 0 0);
        if lc < 3
        then (
          let new_x, new_y, new_dir = get_directions x y direction Left in
          aux new_x new_y new_dir (hl + aai.(y).(x)) 0 (lc + 1) 0);
        if rc < 3
        then (
          let new_x, new_y, new_dir = get_directions x y direction Left in
          aux new_x new_y new_dir (hl + aai.(y).(x)) 0 0 (rc + 1));
        ())
  in
  aux 0 0 E 0 0 0 0;
  !best_route
;;

let input =
  read_lines "inputs/17_t.txt"
  |> remove_empty_string
  |> List.map String.explode
  |> List.map (List.map (String.make 1))
  |> List.map (List.map Int.of_string)
;;

let () = print_newline ()
let max_y = List.length input
let city = Array.make max_y [||]
let () = city_map input city
let () = print_newline ()
let () = print_newline ()
let () = print_aai city
let output = walker city
let () = print_newline ()
let () = print_newline ()

(* Part 1 *)
let part_one () =
  let out_1 = output in
  Printf.printf "Day 17 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  Printf.printf "Day 17 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
