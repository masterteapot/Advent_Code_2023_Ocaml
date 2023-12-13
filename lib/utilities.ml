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

(** Takes a ll [char list list] and a [char] returns the position [int * int] of the char *)
let find_llc ll char =
  let rec aux ll in_count out_count =
    match ll with
    | [] -> failwith ("char ['" ^ String.make 1 char ^ "'] not found")
    | in_list :: tl ->
      (match in_list with
       | [] -> aux tl 0 (out_count + 1)
       | hd :: _ when hd = char -> in_count, out_count
       | _ :: in_tl -> aux (in_tl :: tl) (in_count + 1) out_count)
  in
  aux ll 0 0
;;

(** Takes a list [int list list] and prints the list in a basic format*)
let print_lld a_list =
  print_newline ();
  List.iter
    (fun x ->
      print_newline ();
      List.iter (fun y -> Printf.printf "%d, " y) x;
      print_newline ())
    a_list;
  print_newline ()
;;

(** Takes a ll [char list list] and a position [int * int] and return the char at that position *)
let get_llc ll pos =
  let in_list = List.at ll (snd pos) in
  List.at in_list (fst pos)
;;

(** Takes a list [int list char] and prints the list in a basic format*)
let print_llc a_list =
  print_newline ();
  List.iter
    (fun x ->
      print_newline ();
      List.iter (fun y -> Printf.printf "%c, " y) x;
      print_newline ())
    a_list;
  print_newline ()
;;

(** Takes a [int * int] prints it with an ending new line. No leading newline added. *)
let print_td td = Printf.printf "(%d, %d)\n" (fst td) (snd td)
