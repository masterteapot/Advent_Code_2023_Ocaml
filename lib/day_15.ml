open Batteries
open Utilities

let input = read_lines "inputs/15_t.txt" |> remove_empty_string

let () = print_newline ()
let () = print_newline ()
let () = print_newline ()

(* Part 1 *)
let part_one () =
  let out_1 = 1 in
  Printf.printf "Day 16 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  Printf.printf "Day 15 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
