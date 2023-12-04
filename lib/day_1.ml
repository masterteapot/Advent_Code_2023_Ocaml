open Batteries

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

let outer_digits a_string =
    let digits = String.filter (fun x -> Char.is_digit x) a_string in
    int_of_string((String.left digits 1) ^ (String.right digits 1))
;;

let smarter_digits a_string =
    let rec looper exploded accum =
        match exploded with
        | 'o' :: 'n' :: 'e' :: tl -> looper ('e' :: tl) ('1' :: accum)
        | 't' :: 'w' :: 'o' :: tl -> looper ('o' :: tl) ('2' :: accum)
        | 's' :: 'i' :: 'x' :: tl -> looper ('x' :: tl) ('6' :: accum)
        | 'f' :: 'o' :: 'u' :: 'r' :: tl -> looper ('r' :: tl) ('4' :: accum)
        | 'f' :: 'i' :: 'v' :: 'e' :: tl -> looper ('e' :: tl) ('5' :: accum)
        | 'n' :: 'i' :: 'n' :: 'e' :: tl -> looper ('e' :: tl) ('9' :: accum)
        | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tl -> looper ('e' :: tl) ('3' :: accum)
        | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tl -> looper ('n' :: tl) ('7' :: accum)
        | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tl -> looper ('t' :: tl) ('8' :: accum)
        | hd :: tl -> looper tl (hd :: accum)
        | [] -> List.rev accum in
    let exploded = String.explode a_string in
    let remapped = looper exploded [] in
    let merged = String.of_list remapped in
    outer_digits merged
;;

let in_1 = read_lines "inputs/1.txt" |> remove_empty_string |> List.map outer_digits
let in_2 = read_lines "inputs/1.txt" |> remove_empty_string |> List.map smarter_digits

let out_1 = List.fold_left ( + ) 0 in_1
let out_2 = List.fold_left ( + ) 0 in_2

let main () =
    Printf.printf "Day one part one answer is %d\n" out_1;
    Printf.printf "Day one part two answer is %d\n" out_2
;;
