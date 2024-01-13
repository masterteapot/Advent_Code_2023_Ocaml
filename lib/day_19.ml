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

let get_num = function
  | '0' .. '9' -> true
  | _ -> false
;;

let get_str = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let comma = char ','
let equality = char '='
let integer = take_while1 get_num >>| fun s -> Stdlib.int_of_string s
let get_x = char '{' *> char 'x' *> equality *> integer <* comma >>| fun x -> x
let get_m = char 'm' *> equality *> integer <* comma >>| fun x -> x
let get_a = char 'a' *> equality *> integer <* comma >>| fun x -> x
let get_s = char 's' *> equality *> integer <* char '}'
let get_keys = take_while1 get_str <* char '{' >>| fun x -> x

let sign =
  peek_char
  >>= function
  | Some '>' -> advance 1 >>| fun () -> ">"
  | Some '<' -> advance 1 >>| fun () -> "<"
  | _ -> fail "Sign or digit expected"
;;

let is_rate = function
  | 'a' | 'm' | 's' | 'x' -> true
  | _ -> false
;;

let is_result = function
  | 'a' .. 'z' | 'A' | 'R' -> true
  | _ -> false
;;

let get_result = char ':' *> take_while1 is_result <* char ','
>>| function
| "A" -> Accept
| "R" -> Reject
| x -> Next x

let parse_ratings =
  get_x
  >>= fun x ->
  get_m
  >>= fun m ->
  get_a
  >>= fun a ->
  get_s
  >>| function
  | s -> { x; m; a; s; accepted = false }
;;

let string_of_result = function
  | Accept -> "Accept"
  | Reject -> "Reject"
  | Next x -> x
;;

let parse_workflows =
  get_keys
  >>= fun k ->
  take_while1 is_rate
  >>= fun v ->
  sign
  >>= fun s ->
  integer
  >>= fun i ->
  get_result
  >>| function
  | r -> k ^ " >>> " ^ v ^ s ^ Stdlib.string_of_int i ^ " -> " ^ string_of_result r
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
