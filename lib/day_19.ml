open Base
open Angstrom
open Utilities
open Stdlib.Printf

type action =
  | Reject
  | Accept
  | Next of string

type rating_cat =
  | Extremely_Cool
  | Musical
  | Aerodynamic
  | Shiny

type rating =
  { x : int
  ; m : int
  ; a : int
  ; s : int
  ; accepted : bool
  }

type operation =
  { left : rating_cat
  ; operand : int -> int -> bool
  ; right : int
  }

type workflow =
  { name : string
  ; check : rating -> (string, rating) Base.Hashtbl.t -> rating
  }

let parse_input ls =
  let rec aux ls w acc =
    match ls with
    | "" :: [] -> List.rev w, List.rev acc
    | [] -> List.rev w, List.rev acc
    | "" :: tl -> aux tl acc []
    | hd :: tl -> aux tl w (hd :: acc)
  in
  aux ls [] []
;;

(* let wf = *)
(*   Base.Hashtbl.create *)
(*     ~growth_allowed:true *)
(*     ~size:(List.length workflows_raw) *)
(*     (module struct *)
(*       type t = int * int [@@deriving sexp, compare, hash] *)
(*     end) *)
(* in *)

let input = read_lines "inputs/19_t.txt"
let workflows_raw, ratings_raw = parse_input input

let is_num = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_str = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let comma = char ','
let equality = char '='
let parse_int = take_while1 is_num >>| fun s -> Stdlib.int_of_string s
let parse_x = char '{' *> char 'x' *> equality *> parse_int <* comma >>| fun x -> x
let parse_m = char 'm' *> equality *> parse_int <* comma >>| fun x -> x
let parse_a = char 'a' *> equality *> parse_int <* comma >>| fun x -> x
let parse_s = char 's' *> equality *> parse_int <* char '}'
let parse_keys = take_while1 is_str <* char '{' >>| fun x -> x

let parse_operator =
  peek_char
  >>= function
  | Some '>' -> advance 1 >>| fun () -> ">"
  | Some '<' -> advance 1 >>| fun () -> "<"
  | _ -> fail "Sign or digit expected"
;;

let is_rating = function
  | 'a' | 'm' | 's' | 'x' -> true
  | _ -> false
;;

let is_result = function
  | 'a' .. 'z' | 'A' | 'R' -> true
  | _ -> false
;;

let parse_result =
  char ':' *> take_while1 is_result
  <* char ','
  >>| function
  | "A" -> Accept
  | "R" -> Reject
  | x -> Next x
;;

let parse_end_result =
  take_while1 is_result
  <* char '}'
  >>| function
  | "A" -> Accept
  | "R" -> Reject
  | x -> Next x
;;

let parse_ratings =
  parse_x
  >>= fun x ->
  parse_m
  >>= fun m ->
  parse_a
  >>= fun a ->
  parse_s
  >>| function
  | s -> { x; m; a; s; accepted = false }
;;

let string_of_result = function
  | Accept -> "Accept"
  | Reject -> "Reject"
  | Next x -> x
;;

let parse_operations =
  take_while1 is_rating
  >>= fun left_of_operation ->
  parse_operator
  >>= fun operator ->
  parse_int
  >>= fun right_of_operation ->
  parse_result
  >>| function
  | result -> left_of_operation, operator, right_of_operation, result
;;

let parse_workflows =
  parse_keys
  >>= fun key ->
  parse_operations
  >>| function
  | operation_1 ->
    key
    ^ " >>> "
    ^
      (match operation_1 with
      | lo, o, ro, re -> lo ^ o ^ Stdlib.string_of_int ro ^ " -> " ^ string_of_result re)
;;

let get_results = function
  | Stdlib.Ok x -> x
  | _ -> failwith "NO RESULTS"
;;

let get_result_string = function
  | Stdlib.Ok x -> x
  | _ -> "fail"
;;

let print_ratings r = printf "{x: %d; m: %d; a: %d; s: %d}\n" r.x r.m r.a r.s

let ratings =
  List.map ratings_raw ~f:(fun x ->
    get_results @@ Angstrom.parse_string ~consume:All parse_ratings x)
;;

let workflows =
  List.map workflows_raw ~f:(fun x ->
    get_result_string @@ Angstrom.parse_string ~consume:Prefix parse_workflows x)
;;

let () = Stdlib.print_newline ()
let () = List.iter workflows_raw ~f:Stdlib.print_endline
let () = Stdlib.print_newline ()
let () = List.iter ratings_raw ~f:Stdlib.print_endline
let () = Stdlib.print_newline ()
let () = List.iter ratings ~f:print_ratings
let () = Stdlib.print_newline ()
let () = Stdlib.print_newline ()
let () = Stdlib.print_newline ()
let () = List.iter workflows ~f:Stdlib.print_endline

(* Part 1 *)
let part_one () =
  let out_1 = 1 in
  printf "Day 19 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  printf "Day 19 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
