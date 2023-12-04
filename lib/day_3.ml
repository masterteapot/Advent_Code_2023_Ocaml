open Batteries

type p_num = {
    num : int;
    x_start : int;
    x_end : int;
    y : int;
    mutable valid : bool
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
                    ({
    num = int_of_string num;
    x_start;
    x_end = counter - 1;
    y;
    valid = false
} :: output)
        | _ :: tl ->
                looper 
                    (counter + 1)
                    tl
                    y
                    0
                    ""
                    output
        | [] -> output in
    looper 0 scheme y 0 "" []
;;

let validate_p schema p =
    let x_min = p.x_start - 1 in
    let x_max = p.x_end + 1 in
    let y_min = p.y - 1 in
    let y_max = p.y + 1 in
    let sorrounding_schema = List.filteri_map 
        (fun i y -> 
            if i >= y_min && i <= y_max 
            then 
                Some (List.filteri_map (fun i x -> if i >= x_min && i <= x_max then Some x else None) y) 
            else None) 
        schema in
    let sorrounding_symbols = sorrounding_schema |> List.flatten |> List.filter (Char.is_symbol) in
    let is_valid = (List.length sorrounding_symbols) > 0 in
    p.valid <- is_valid
;;

let sum_parts a b =
    if b.valid then a + b.num
    else a

let in_1 = read_lines "inputs/3.txt" |> remove_empty_string |> List.map String.explode
(* let in_2 = read_lines "inputs/3.txt" |> remove_empty_string  *)
let parts = List.mapi make_nums in_1 |> List.flatten
let () = List.iter (validate_p in_1) parts

let out_1 = List.fold_left sum_parts 0 parts
let out_2 = 2

let main () =
    Printf.printf "Day two part one answer is %d\n" out_1;
    Printf.printf "Day two part two answer is %d\n" out_2
;;
