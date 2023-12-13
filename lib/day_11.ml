open Batteries
(* open Utilities *)

let sort_td_fst_asc x y = if fst x > fst y then 1 else if fst x = fst y then 0 else -1
let sort_td_snd_asc x y = if snd x > snd y then 1 else if snd x = snd y then 0 else -1

let expanding_universe exp_calc galaxies =
  let rec aux_horizontal galaxies last_x expansion acc =
    match galaxies with
    | [] -> List.rev acc |> List.sort sort_td_fst_asc |> List.sort sort_td_snd_asc
    | (x, y) :: tl when x + expansion - last_x < 2 ->
      aux_horizontal tl (x + expansion) expansion ((x + expansion, y) :: acc)
    | (x, y) :: tl ->
      let new_expanse = exp_calc expansion last_x x in
      aux_horizontal tl (x + new_expanse) new_expanse ((x + new_expanse, y) :: acc)
  in
  let rec aux_vertical galaxies last_y expansion acc =
    match galaxies with
    | [] -> List.rev acc
    | (x, y) :: tl when y + expansion - last_y < 2 ->
      aux_vertical tl (y + expansion) expansion ((x, y + expansion) :: acc)
    | (x, y) :: tl ->
      let new_expanse = exp_calc expansion last_y y in
      aux_vertical tl (y + new_expanse) new_expanse ((x, y + new_expanse) :: acc)
  in
  let new_galaxies = aux_vertical (List.sort sort_td_snd_asc galaxies) 0 0 [] in
  aux_horizontal (List.sort sort_td_fst_asc new_galaxies) 0 0 []
;;

let old_expansion_calc expansion old_v v = v + expansion - 1 - old_v + expansion

let new_expansion_calc expansion old_v v =
  let gap = v + expansion - 1 - old_v in
  (gap * 1000000) + expansion - gap
;;

let pairs galaxies =
  let rec make_pairs galaxies acc =
    match galaxies with
    | [] -> acc
    | hd :: md :: tl -> make_pairs (hd :: tl) ((hd, md) :: acc)
    | _ :: tl -> make_pairs tl acc
  in
  let rec aux galaxies acc =
    match galaxies with
    | [] -> acc
    | hd :: tl -> aux tl (make_pairs (hd :: tl) [] :: acc)
  in
  aux galaxies [] |> List.flatten
;;

let calc_g_dist ttd =
  match ttd with
  | (a, b), (x, y) -> abs (a - x) + abs (b - y)
;;

let input = Utilities.read_lines "inputs/11.txt" |> Utilities.remove_empty_string

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
  |> List.sort sort_td_fst_asc
  |> List.sort sort_td_snd_asc
;;

(* Part 1 *)
let part_one () =
  let new_galaxies = expanding_universe old_expansion_calc galaxies in
  (* List.iter print_td galaxies; *)
  let paired_galaxies = pairs new_galaxies in
  let out_1 = List.map calc_g_dist paired_galaxies |> List.fold_left ( + ) 0 in
  Printf.printf "Day 11 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let new_galaxies = expanding_universe new_expansion_calc galaxies in
  (* List.iter print_td new_galaxies; *)
  let paired_galaxies = pairs new_galaxies in
  let out_2 = List.map calc_g_dist paired_galaxies |> List.fold_left ( + ) 0 in
  Printf.printf "Day 11 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
