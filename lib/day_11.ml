open Batteries

let input = Utilities.read_lines "inputs/11_t.txt" |> Utilities.remove_empty_string
let () = print_newline ()
let () = print_newline ()
let () = List.iter print_endline input
let () = print_newline ()

let galaxies =
  List.mapi
    (fun y ll -> List.mapi (fun x c -> if c = '#' then Some (x, y) else None) ll)
    (List.map String.explode input)
  |> List.map (List.filter (fun x -> x <> None))
  |> List.filter (fun x -> List.length x > 0)
  |> List.flatten
  |> List.map (fun x ->
    match x with
    | Some x -> x
    | _ -> failwith "found a none value in the galaxies")
;;

let max_x = String.length (List.hd input)
let max_y = List.length input

let expanding_universe galaxies =
    let rec aux_vertical galaxies last_y expansion acc =
        match galaxies with
        | [] -> acc
        | (x, y) :: tl when y = last_y -> 
                aux_vertical tl y expansion ((x, y + expansion) :: acc)
        | (x, y) :: tl when y = (last_y + 1) -> 
                aux_vertical tl y expansion ((x, y + expansion) :: acc)
        | (x, y) :: tl when y > (last_y + 1) ->
                let new_expanse = y - last_y + 1 + expansion  in
                aux_vertical tl y new_expanse ((x, y + new_expanse) :: acc)
        | _ -> failwith "why are we here in the emptyness of space?" in
    aux_vertical galaxies 0 0 []
;;

let new_galaxies = expanding_universe galaxies

(* Part 1 *)
let part_one () =
  let out_1 = 1 in
  Printf.printf "Day 11 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  Printf.printf "Day 11 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
