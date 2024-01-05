open Base
open Utilities
open Stdlib.Printf

(* open Core *)

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
    let arr = Array.create ~len:(List.length ls) default in
    List.iteri ~f:(Array.set arr) ls;
    arr
;;

let city_map input aai =
  List.iteri ~f:(fun i x -> Array.set aai i (array_of_list x)) input
;;

let get_directions x y d m =
  match d, m with
  | N, Left -> Int.pred x, y, W
  | E, Left -> x, Int.pred y, N
  | S, Left -> Int.succ x, y, E
  | W, Left -> x, Int.succ y, S
  | N, Right -> Int.succ x, y, E
  | E, Right -> x, Int.succ y, S
  | S, Right -> Int.pred x, y, W
  | W, Right -> x, Int.pred y, N
  | N, Straight -> x, Int.pred y, N
  | E, Straight -> Int.succ x, y, E
  | S, Straight -> x, Int.succ y, S
  | W, Straight -> Int.pred x, y, W
;;

let calc_distance start finish =
  let x_dist = Int.abs @@ (fst start - fst finish) in
  let y_dist = Int.abs @@ (snd start - snd finish) in
  x_dist + y_dist
;;

let rec update_ordered_list x ls acc =
  match ls with
  | [] -> List.rev (x :: acc)
  | hd :: tl when x < hd -> update_ordered_list x tl (hd :: acc)
  | hd :: tl when x >= hd -> List.rev (x :: acc) @ (hd :: tl)
  | _ -> failwith "The logic for reverse lists is wrong"
;;

let walker max_paths max_ref aai best_progress =
  let max_y = Int.pred @@ Array.length aai in
  let max_x = Int.pred @@ Array.length aai.(0) in
  let best_route = ref max_ref in
  let rec aux x y direction heat sc =
    let dist = calc_distance (x, y) (max_x, max_y) in
    let bp = best_progress.(dist) in
    let bp_length = List.length bp in
    if x < 0
       || x > max_x
       || y < 0
       || y > max_y
       || (bp_length = max_paths
           && heat
              >=
              match List.hd bp with
              | Some x -> x
              | None -> failwith "Not expecting empty list")
    then ()
    else (
      let bp =
        if bp_length = max_paths
        then (
          match List.tl bp with
          | Some x -> x
          | None -> failwith "Not expecting empty list")
        else bp
      in
      (match bp with
       | [] -> Array.set best_progress dist [ heat ]
       | bp -> Array.set best_progress dist (update_ordered_list heat bp []));
      match x, y with
      | x, y when x = max_x && y = max_y ->
        if heat + aai.(y).(x) - aai.(0).(0) < !best_route
        then best_route := heat + aai.(y).(x) - aai.(0).(0)
        else ()
      | _ when heat >= !best_route -> ()
      | x, y ->
        if sc < 2
        then (
          let new_x, new_y, new_dir = get_directions x y direction Straight in
          aux new_x new_y new_dir (heat + aai.(y).(x)) (sc + 1));
        let new_x, new_y, new_dir = get_directions x y direction Left in
        aux new_x new_y new_dir (heat + aai.(y).(x)) 0;
        let new_x, new_y, new_dir = get_directions x y direction Right in
        aux new_x new_y new_dir (heat + aai.(y).(x)) 0)
  in
  aux 0 0 S 0 0;
  !best_route
;;

let input =
  read_lines "inputs/17_t.txt"
  |> remove_empty_string
  |> List.map ~f:String.to_list
  |> List.map ~f:(List.map ~f:(String.make 1))
  |> List.map ~f:(List.map ~f:Int.of_string)
;;

let () = Stdlib.print_newline ()
let max_y = List.length input
let city = Array.create ~len:max_y [||]
let () = city_map input city
let () = Stdlib.print_newline ()
let () = Stdlib.print_newline ()

(* let () = print_aai city *)
let max_y = Int.pred @@ Array.length city
let max_x = Int.pred @@ Array.length city.(0)
let best_progress = Array.create ~len:(max_y * max_x) []
let max_paths = List.init 8 ~f:(fun x -> Int.pow (x + 1) 4)

let output =
  List.fold_left max_paths ~init:1000 ~f:(fun acc x -> walker x acc city best_progress)
;;

let () = Stdlib.print_newline ()
let () = Stdlib.print_newline ()

(* last entry was 993 *)

(* Part 1 *)
let part_one () =
  let out_1 = output in
  printf "Day 17 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  printf "Day 17 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
