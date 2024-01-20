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
  ; output : action
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
  | Some '>' -> advance 1 >>| fun () -> Stdlib.( > )
  | Some '<' -> advance 1 >>| fun () -> Stdlib.( < )
  | _ -> failwith "Sign or digit expected"
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

let parse_rating =
  take_while1 is_rating
  >>= fun r ->
  peek_char
  >>= fun n ->
  (match n with
   | Some '>' -> return r
   | Some '<' -> return r
   | _ -> fail "Hey this isn't a rating")
  >>| function
  | "a" -> Aerodynamic
  | "m" -> Musical
  | "s" -> Shiny
  | "x" -> Extremely_Cool
  | _ -> failwith "Not a real rating"
;;

let string_of_result = function
  | Accept -> "Accept"
  | Reject -> "Reject"
  | Next x -> x
;;

let string_of_rating = function
  | Aerodynamic -> "Aerodynamic"
  | Musical -> "Musical"
  | Shiny -> "Shiny"
  | Extremely_Cool -> "Extremely Cool"
;;

let parse_operations =
  parse_rating
  >>= fun left ->
  parse_operator
  >>= fun operand ->
  parse_int
  >>= fun right ->
  parse_result
  >>| function
  | output -> Some { left; operand; right; output }
;;

let parse_workflows =
  parse_keys
  >>= fun key ->
  parse_operations
  <|> return None
  >>= fun operation_1 ->
  parse_operations
  <|> return None
  >>= fun operation_2 ->
  parse_operations
  <|> return None
  >>= fun operation_3 ->
  parse_operations
  <|> return None
  >>= fun operation_4 ->
  parse_end_result
  >>| function
  | return_value ->
    let r_fun r =
      let get_r_value o =
        match o with
        | Shiny -> r.s
        | Extremely_Cool -> r.x
        | Aerodynamic -> r.a
        | Musical -> r.m
      in
      let doctor o = o.operand (get_r_value o.left) o.right in
      match operation_1, operation_2, operation_3, operation_4 with
      | Some a, Some b, Some c, Some d ->
        if doctor a
        then a.output
        else if doctor b
        then b.output
        else if doctor c
        then c.output
        else if doctor d
        then d.output
        else return_value
      | Some a, Some b, Some c, _ ->
        if doctor a
        then a.output
        else if doctor b
        then b.output
        else if doctor c
        then c.output
        else return_value
      | Some a, Some b, _, _ ->
        if doctor a then a.output else if doctor b then b.output else return_value
      | Some a, _, _, _ -> if doctor a then a.output else return_value
      | _ -> failwith "Didn't expect this"
    in
    key, r_fun
;;

let rate_em wf r =
  let rec aux r = function
    | Accept -> { r with accepted = true }
    | Reject -> { r with accepted = false }
    | Next x -> aux r ((Hashtbl.find_exn wf x) r)
  in
  aux r (Next "in")
;;

let get_results = function
  | Stdlib.Ok x -> x
  | _ -> failwith "NO RESULTS"
;;

let get_result_string = function
  | Stdlib.Ok x -> x
  | _ -> "fail"
;;

let print_ratings r =
  printf "{x: %d; m: %d; a: %d; s: %d; accepted: %b}\n" r.x r.m r.a r.s r.accepted
;;

(* Part 1 *)
let part_one () =
  let input = read_lines "inputs/19.txt" in
  let workflows_raw, ratings_raw = parse_input input in
  let ratings =
    List.map ratings_raw ~f:(fun x ->
      get_results @@ Angstrom.parse_string ~consume:All parse_ratings x)
  in
  let workflows =
    List.map workflows_raw ~f:(fun x ->
      get_results @@ Angstrom.parse_string ~consume:Prefix parse_workflows x)
  in
  let wf = Hashtbl.create (module String) in
  let () = List.iter workflows ~f:(fun x -> Hashtbl.set wf ~key:(fst x) ~data:(snd x)) in
  let () = Stdlib.print_newline () in
  let new_ratings = List.map ratings ~f:(rate_em wf) in
  let sum_of_ratings =
    List.fold_left new_ratings ~init:0 ~f:(fun acc x ->
      acc + if x.accepted then x.x + x.m + x.a + x.s else 0)
  in
  let out_1 = sum_of_ratings in
  printf "Day 19 Part 1 --> %d\n" out_1
;;

let main () =
  part_one ();
;;
