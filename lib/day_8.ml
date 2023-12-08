open Batteries

type tree =
  | Deadend
  | Node of node

and node =
  { name : string
  ; left : tree
  ; right : tree
  }

type instruction =
  | R
  | L

let print_node a_node =
  let name =
    match a_node with
    | Node { name; left; right } when left <> Deadend && right <> Deadend -> name
    | Deadend -> "None found"
    | _ -> "None found"
  in
  Printf.printf "Name: %s \n" name
;;

let make_node all_strings =
  let rec find_nodes all_strings search_str =
    let a_str =
      List.find (fun x -> String.starts_with x (search_str ^ " =")) all_strings
    in
    let parts = String.split_on_string ~by:" = " a_str in
    let name = List.hd parts in
    let to_nodes = List.at parts 1 |> String.explode in
    let rec get_nodes left right acc to_nodes =
      match to_nodes with
      | [] -> left, right
      | ',' :: tl -> get_nodes acc right "" tl
      | ')' :: tl -> get_nodes left acc "" tl
      | x :: tl when Char.is_letter x -> get_nodes left right (acc ^ String.make 1 x) tl
      | _ :: tl -> get_nodes left right "" tl
    in
    let left_name, right_name = get_nodes "" "" "" to_nodes in
    match left_name, right_name with
    | ln, rn when ln = name && rn = name -> Node { name; left = Deadend; right = Deadend }
    | ln, rn when rn = name ->
      Node { name; left = find_nodes all_strings ln; right = Deadend }
    | ln, rn when ln = name ->
      Node { name; left = Deadend; right = find_nodes all_strings rn }
    | ln, rn ->
      Node { name; left = find_nodes all_strings ln; right = find_nodes all_strings rn }
  in
  find_nodes all_strings "AAA"
;;

(** [mem x t] is whether [x] is a value at some node in tree [t]. *)
let rec mem x = function
  | Deadend -> false
  | Node { name; left; right } -> name = x || mem x left || mem x right
;;

let preorder a_tree =
  let rec pre_acc acc = function
    | Deadend -> acc
    | Node { name; left; right } -> name :: pre_acc (pre_acc acc right) left
  in
  pre_acc [] a_tree
;;

let input = Utilities.read_lines "inputs/8_t.txt" |> Utilities.remove_empty_string
let () = print_newline ()
let () = List.iter print_endline input
let () = print_newline ()

(* Part 1 *)
let instructions = String.explode (List.hd input)
let noders = make_node (List.tl input)


(* Part 2 *)
let input2 = Utilities.read_lines "inputs/8_t2.txt" |> Utilities.remove_empty_string
let instruct2 = String.explode (List.hd input2)
let noders2 = make_node (List.tl input2)

let main () =
  let out_1 = 1 in
  let out_2 = 2 in
  Printf.printf "Day 4 Part 1 --> %d\n" out_1;
  Printf.printf "Day 4 Part 2 --> %d\n" out_2
;;
