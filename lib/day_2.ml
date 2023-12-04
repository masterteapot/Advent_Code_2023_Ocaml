open Batteries

type game_max =
  { red : int
  ; green : int
  ; blue : int
  }

type game =
  { index : int
  ; mutable max_blue : int
  ; mutable max_red : int
  ; mutable max_green : int
  ; mutable is_possible : bool
  }

(* only 12 red cubes, 13 green cubes, and 14 blue cubes *)
let my_max = { red = 12; green = 13; blue = 14 }

let read_lines filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
;;

let rec remove_empty_string a_list =
  match a_list with
  | [] -> []
  | a_str :: tl ->
    (match a_str with
     | "" -> remove_empty_string tl
     | a_str -> a_str :: remove_empty_string tl)
;;

let parse_inputs max_values g_string =
  let update_game a_game a_pick =
    let pick = String.split_on_char ' ' a_pick in
    let p_group =
      match pick with
      | [ num; color ] -> int_of_string num, color
      | _ -> failwith "num of balls error"
    in
    match p_group with
    | num, color when color = "blue" && num > a_game.max_blue -> a_game.max_blue <- num
    | num, color when color = "green" && num > a_game.max_green -> a_game.max_green <- num
    | num, color when color = "red" && num > a_game.max_red -> a_game.max_red <- num
    | _ -> ()
  in
  let g_split = String.split_on_string ~by:": " g_string in
  let g_index =
    String.split_on_char ' ' (List.hd g_split) |> List.last |> int_of_string
  in
  let g_rounds = List.last g_split |> String.split_on_char ';' in
  let g_picks =
    List.map (fun x -> String.split_on_char ',' x) g_rounds
    |> List.flatten
    |> List.map String.strip
  in
  let g =
    { index = g_index; max_blue = 0; max_red = 0; max_green = 0; is_possible = true }
  in
  List.iter (update_game g) g_picks;
  if g.max_blue > max_values.blue
     || g.max_green > max_values.green
     || g.max_red > max_values.red
  then g.is_possible <- false;
  g
;;

let rec add_ids all_games =
  match all_games with
  | hd :: tl when hd.is_possible = true -> hd.index + add_ids tl
  | _ :: tl -> add_ids tl
  | [] -> 0
;;

let get_powers a_game = a_game.max_red * a_game.max_blue * a_game.max_green

let in_1 =
  read_lines "inputs/2.txt" |> remove_empty_string |> List.map (parse_inputs my_max)
;;

let in_2 =
  read_lines "inputs/2.txt" |> remove_empty_string |> List.map (parse_inputs my_max)
;;

let out_1 = add_ids in_1
let out_2 = List.map get_powers in_2 |> List.fold_left ( + ) 0

let main () =
  Printf.printf "Day 2 Part 1 --> %d\n" out_1;
  Printf.printf "Day 2 Part 2 --> %d\n" out_2
;;
