open Batteries
(* open Utilities *)

let explode_puzzles puzzle =
  let rec aux puzzle shrinking_puzzle vert_chars acc =
    match puzzle with
    | [] when List.length (List.hd shrinking_puzzle) = 0 -> List.rev (List.map List.rev (vert_chars :: acc))
    | [] -> aux shrinking_puzzle [] [] (List.rev vert_chars :: acc)
    | chars :: otl ->
      (match chars with
       | [] -> failwith "this shouldn't happen"
       | c :: tl -> aux otl (tl :: shrinking_puzzle) (c :: vert_chars) acc)
  in
  let v_input = List.map String.explode puzzle in
  let v_puzzle = aux v_input [] [] [] |> List.map String.of_list in
  puzzle, v_puzzle
;;

let find_solution input =
    let rec aux input counter =
        match input with
        | hd :: md :: _ when hd = md -> counter + 1
        | _ :: tl -> aux tl (counter + 1) 
        | [] -> failwith "no solution found"
    in
    aux input 0
;;

let input = Utilities.read_lines "inputs/13_t1.txt" |> Utilities.remove_empty_string
let t = explode_puzzles input
let t2 = find_solution (fst t)
let () = print_newline ()
let () = print_newline ()
let () = List.iter print_endline (fst t)
let () = print_newline ()
let () = List.iter (fun x -> print_int @@ String.length x) (fst t)
let () = print_newline ()
let () = List.iter print_endline (snd t)
let () = print_newline ()
let () = List.iter (fun x -> print_int @@ String.length x) (snd t)
let () = print_newline ()
let () = print_newline ()
let () = print_int t2
let () = print_newline ()

(* Part 1 *)
let part_one () =
  let out_1 = 1 in
  Printf.printf "Day 13 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  Printf.printf "Day 13 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
