open Base
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
    | "" :: tl -> aux tl acc []
    | [] -> List.rev w, List.rev acc
    | hd :: tl -> aux tl w (hd :: acc)
  in
  aux ls [] []
;;

let input = read_lines "inputs/19_t.txt" |> remove_empty_string;;

let workflows_raw, ratings_raw = parse_input input in
let wf =
  Base.Hashtbl.create
    ~growth_allowed:true
    ~size:(List.length workflows_raw)
    (module struct
      type t = int * int [@@deriving sexp, compare, hash]
    end)
in
Stdlib.print_newline ();
Stdlib.print_newline ();
List.iter input ~f:Stdlib.print_endline

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
