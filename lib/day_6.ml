open Batteries

type race_leg =
  { time : int
  ; distance : int
  ; mutable num_winners : int
  }

let get_digits raw =
  let rec aux raw inner out =
    match raw with
    | [] -> List.rev out
    | x :: [] when Char.is_digit x -> List.rev ((inner ^ String.make 1 x) :: out)
    | x :: tl when Char.is_digit x -> aux tl (inner ^ String.make 1 x) out
    | x :: tl when (not (Char.is_digit x)) && String.length inner > 0 ->
      aux tl "" (inner :: out)
    | _ :: tl -> aux tl "" out
  in
  aux raw "" []
;;

let get_races digits =
  assert (List.length digits = 2);
  let times = List.at digits 0 in
  let distances = List.at digits 1 in
  let rec aux times distances acc =
    match times, distances with
    | [], [] -> List.rev acc
    | time :: xt, distance :: yt -> aux xt yt ({ time; distance; num_winners = 0 } :: acc)
    | _, _ -> failwith "something isn't right"
  in
  aux times distances []
;;

let get_races_v2 digits =
  assert (List.length digits = 2);
  let time = List.at digits 0 in
  let distance = List.at digits 1 in
  { time; distance; num_winners = 0 }
;;

let print_race race =
  Printf.printf
    "Time: %d || Distance: %d || Num Winners: %d \n"
    race.time
    race.distance
    race.num_winners
;;

let calc_time time race =
  let speed = time in
  let remaining_time = race.time - time in
  speed * remaining_time
;;

let run_races race =
  let rec aux counter =
    match counter with
    | _ when counter > race.time -> race
    | x ->
      let race_distance = calc_time x race in
      if race_distance > race.distance then race.num_winners <- race.num_winners + 1;
      aux (counter + 1)
  in
  aux 1
;;

let get_digits_single raw =
  let rec aux raw out =
    match raw with
    | [] -> int_of_string out
    | x :: tl when Char.is_digit x -> aux tl (out ^ String.make 1 x)
    | _ :: tl -> aux tl out
  in
  aux raw ""
;;

let input = Utilities.read_lines "inputs/6.txt" |> Utilities.remove_empty_string
let exploded = List.map String.explode input

(* Part 1 *)
let digits = List.map get_digits exploded |> List.map (fun x -> List.map int_of_string x)
let races = get_races digits |> List.map run_races

(* Part 2 *)
let digi_v2 = List.map get_digits_single exploded
let race_v2 = get_races_v2 digi_v2 |> run_races

let main () =
  let out_1 = List.fold_left (fun acc x -> acc * x.num_winners) 1 races in
  let out_2 = race_v2.num_winners in
  Printf.printf "Day 4 Part 1 --> %d\n" out_1;
  Printf.printf "Day 4 Part 2 --> %d\n" out_2
;;
