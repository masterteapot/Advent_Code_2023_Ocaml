open Batteries

type p_num =
  { num : int
  ; x_start : int
  ; x_end : int
  ; y : int
  ; mutable valid : bool
  }

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

let make_nums y scheme =
  let rec looper counter scheme y x_start num output =
    match scheme with
    | [] -> List.rev output
    | hd :: [] when Char.is_digit hd && String.length num > 0 ->
      looper
        0
        []
        y
        0
        ""
        ({ num = int_of_string (num ^ String.make 1 hd)
         ; x_start
         ; x_end = counter
         ; y
         ; valid = false
         }
         :: output)
    | hd :: tl when Char.is_digit hd ->
      looper
        (counter + 1)
        tl
        y
        (if String.length num = 0 then counter else x_start)
        (num ^ String.make 1 hd)
        output
    | _ :: tl when String.length num > 0 ->
      looper
        (counter + 1)
        tl
        y
        0
        ""
        ({ num = int_of_string num
         ; x_start
         ; x_end = counter - 1
         ; y
         ; valid = false
         }
         :: output)
    | _ :: tl -> looper (counter + 1) tl y 0 "" output
  in
  looper 0 scheme y 0 "" []
;;

let validate_p schema p =
  let x_min = p.x_start - 1 in
  let x_max = p.x_end + 1 in
  let y_min = p.y - 1 in
  let y_max = p.y + 1 in
  let sorrounding_schema =
    List.filteri_map
      (fun i y ->
        if i >= y_min && i <= y_max
        then
          Some
            (List.filteri_map
               (fun i x -> if i >= x_min && i <= x_max then Some x else None)
               y)
        else None)
      schema
  in
  let sorrounding_symbols =
    sorrounding_schema |> List.flatten |> List.filter Char.is_symbol
  in
  let is_valid = List.length sorrounding_symbols > 0 in
  p.valid <- is_valid
;;

let get_gears y scheme =
  let rec looper counter scheme y output =
    match scheme with
    | '*' :: tl -> looper (counter + 1) tl y ((counter, y) :: output)
    | _ :: tl -> looper (counter + 1) tl y output
    | [] -> List.rev output
  in
  looper 0 scheme y []
;;

let gear_parts parts g =
  List.find_all
    (fun p ->
      p.y <= snd g + 1
      && p.y >= snd g - 1
      && p.x_end >= fst g - 1
      && p.x_start <= fst g + 1)
    parts
;;

let sum_parts a b = if b.valid then a + b.num else a
let input = read_lines "inputs/3.txt" |> remove_empty_string |> List.map String.explode
let parts = List.mapi make_nums input |> List.flatten
let () = List.iter (validate_p input) parts
let out_1 = List.fold_left sum_parts 0 parts
let valid_parts = List.filter (fun x -> x.valid) parts
let gears = List.mapi get_gears input |> List.flatten

let g_parts =
  List.map (gear_parts valid_parts) gears |> List.filter (fun x -> List.length x = 2)
;;

let p_nums =
  List.map
    (fun x ->
      match x with
      | [ one; two ] -> one.num * two.num
      | _ -> failwith "we should only have two parts")
    g_parts
;;

let out_2 = List.fold_left ( + ) 0 p_nums

let main () =
  Printf.printf "Day 3 Part 1 --> %d\n" out_1;
  Printf.printf "Day 3 Part 2 --> %d\n" out_2
;;
