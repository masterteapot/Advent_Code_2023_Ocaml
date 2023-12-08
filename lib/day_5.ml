open Batteries

type seed_map =
  { from_name : string
  ; to_name : string
  ; ranges : (int * int * int) list
  }

let get_maps input =
  let rec looper input inner outer =
    match input with
    | [] -> List.rev (List.rev inner :: outer)
    | hd :: tl when hd.[0] |> Char.is_digit -> looper tl (hd :: inner) outer
    | hd :: tl when List.length inner > 0 -> looper tl [ hd ] (List.rev inner :: outer)
    | hd :: tl -> looper tl (hd :: inner) outer
  in
  looper input [] []
;;

let make_map map =
  let split = String.split_on_string ~by:"-to-" (List.hd map) in
  let from_name = List.hd split in
  let to_name = List.last split |> String.split_on_char ' ' |> List.hd in
  let ranges =
    List.tl map
    |> List.map (String.split_on_char ' ')
    |> List.map (fun x ->
      match x with
      | [ tv; fv; rg ] -> int_of_string tv, int_of_string fv, int_of_string rg
      | [] -> 0, 0, 0
      | x ->
        List.iter print_endline x;
        failwith "not three ln list")
  in
  { from_name; to_name; ranges }
;;

let in_n_out in_value map =
  let rec looper ranges =
    match ranges with
    | [] -> in_value, map.to_name
    | (tv, fv, rg) :: _ when 
    in_value >= fv 
    && in_value < fv + rg ->
      in_value - fv + tv, map.to_name
    | _ :: tl -> looper tl
  in
  looper map.ranges
;;

let traverse_maps maps start_value =
  let rec looper maps from_value from_name =
    match maps with
    | [] -> from_value
    | hd :: tl when hd.from_name = from_name ->
      let new_value, new_name = in_n_out from_value hd in
      looper tl new_value new_name
    | hd :: _ ->
      print_endline ("\n hd: " ^ hd.from_name);
      print_endline ("\n from_name: " ^ from_name);
      failwith "Don't know how we got here"
  in
  looper maps start_value "seed"
;;

(* - - - - Part 1 - - - -  *)
let input = Utilities.read_lines "inputs/5.txt" |> Utilities.remove_empty_string
let sorted = get_maps input

let seeds =
  sorted
  |> List.hd
  |> List.hd
  |> String.split_on_char ' '
  |> List.filter (fun x ->
    match x with
    | "seeds:" -> false
    | _ -> true)
  |> List.map (fun v -> int_of_string v)
;;

let raw_maps = List.tl sorted
let maps = List.map make_map raw_maps

let results =
  List.map (traverse_maps maps) seeds
  |> List.fold_left (fun acc x -> if x < acc then x else acc) 100000000
;;

let out_1 = results

(* - - - - Part 2 - - - -  *)

let seed_me seeds =
  let rec looper seeds acc =
    match seeds with
    | [] -> acc
    | start :: len :: tl ->
      let ns = start, start + len - 1 in
      looper tl (ns :: acc)
    | _ :: _ -> failwith "we should only have pairs, I guess not..."
  in
  looper seeds []
;;

let get_from_ranges min_start max_start map_range =
  let mf_min = Tuple3.second map_range in
  let mf_max = Tuple3.third map_range + mf_min - 1 in
  match min_start, max_start with
  | ms, xs when xs < mf_min || ms > mf_max || (ms >= mf_min && xs <= mf_max) -> [ ms, xs ]
  | ms, xs when ms < mf_min && xs <= mf_max -> [ mf_min, xs; ms, mf_min - 1 ]
  | ms, xs when ms >= mf_min && xs > mf_max -> [ ms, mf_max; mf_max + 1, xs ]
  | ms, xs when ms < mf_min && xs > mf_max ->
    [ mf_min, mf_max; ms, mf_min - 1; mf_max + 1, xs ]
  | ms, xs ->
    Printf.printf "\nms: %d; xs: %d; mf_min: %d; mf_max: %d\n" ms xs mf_min mf_max;
    failwith "we should have covered everything"
;;

let loop_map_ranges min_start max_start map =
  let map_ranges = map.ranges in
  List.map (get_from_ranges min_start max_start) map_ranges |> List.flatten |> List.unique
;;

let loop_from_ranges from_values map =
  let rec looper from_values map acc =
    match from_values with
    | [] -> acc |> List.rev |> List.flatten
    | (mf, xf) :: tl ->
      let nr = loop_map_ranges mf xf map in
      looper tl map (nr :: acc)
  in
  looper from_values map []
;;

let in_n_out_tupler map from_tuple =
  let mt = in_n_out (fst from_tuple) map |> fst in
  let xt = in_n_out (snd from_tuple) map |> fst in
  let out = if mt < xt then mt, xt else xt, mt in
  Printf.printf
    "In: (%d, %d) || Out: (%d, %d)\n"
    (fst from_tuple)
    (snd from_tuple)
    (fst out)
    (snd out);
  out
;;

let merge values =
  let rec acc_loop v old_acc new_acc =
    match old_acc with
    | (n, x) :: tl when n > snd v || x < fst v -> acc_loop v tl ((n, x) :: new_acc)
    | (n, x) :: tl ->
      let new_v = Int.min n (fst v), Int.max x (snd v) in
      acc_loop new_v tl new_acc
    | [] -> v :: new_acc
  in
  let rec value_loop values acc =
    match values with
    | [] -> acc
    | hd :: tl ->
      let new_acc = acc_loop hd acc [] in
      value_loop tl new_acc
  in
  value_loop values []
;;

let traverse_mappers maps from_values seed_name =
  let rec looper maps from_values from_name =
    match maps with
    | [] -> from_values
    | hd :: [] when hd.from_name = from_name ->
      let all_froms = loop_from_ranges from_values hd in
      let all_tos = List.map (in_n_out_tupler hd) all_froms in
      Printf.printf
        "\n At Seedname: %s; We have list of all tos : %d\n"
        hd.to_name
        (List.length all_tos);
      List.iter (fun x -> Printf.printf "(%d, %d)\n" (fst x) (snd x)) all_tos;
      looper [] all_tos hd.to_name
    | hd :: tl when hd.from_name = from_name ->
      let all_froms = loop_from_ranges from_values hd in
      let all_tos = List.map (in_n_out_tupler hd) all_froms |> merge in
      Printf.printf
        "\n At Seedname: %s; We have list of all tos : %d\n"
        hd.to_name
        (List.length all_tos);
      List.iter (fun x -> Printf.printf "(%d, %d)\n" (fst x) (snd x)) all_tos;
      looper tl all_tos hd.to_name
    | hd :: _ ->
      print_endline ("\n hd: " ^ hd.from_name);
      print_endline ("\n from_name: " ^ from_name);
      failwith "Don't know how we got here"
  in
  looper maps from_values seed_name
;;

let main () =
  (* Fast solution *)
  let twoput = Utilities.read_lines "inputs/5_t.txt" |> Utilities.remove_empty_string in
  let tworted = get_maps twoput in
  let tweeds =
    tworted
    |> List.hd
    |> List.hd
    |> String.split_on_char ' '
    |> List.filter (fun x ->
      match x with
      | "seeds:" -> false
      | _ -> true)
    |> List.map (fun v -> int_of_string v)
    |> seed_me
  in
  let two_raw_maps = List.tl tworted in
  let two_maps = List.map make_map two_raw_maps in
  print_endline "\n We are about to start traversing";
  let output = traverse_mappers two_maps tweeds "seed" in
  let out_2 =
    List.fold_left (fun acc x -> if fst x < acc then fst x else acc) 9000000000 output
  in
  (* Slow solution *)
  (* let out_2 = *)
  (*   List.fold_left *)
  (*     (fun acc seed_pair -> *)
  (*       let start = fst seed_pair in *)
  (*       let finish = fst seed_pair + snd seed_pair in *)
  (*       let rec run_range seed acc = *)
  (*         if seed > finish *)
  (*         then acc *)
  (*         else ( *)
  (*           let location = traverse_maps two_maps seed in *)
  (*           if location < acc *)
  (*           then run_range (seed + 1) location *)
  (*           else run_range (seed + 1) acc) *)
  (*       in *)
  (*       let lowest = run_range start finish in *)
  (*       if lowest < acc then lowest else acc) *)
  (*     100000000000 *)
  (*     tweeds *)
  (* in *)
  (* Correct answer For Part 2 is 34039469 *)
  Printf.printf "Day 4 Part 1 --> %d\n" out_1;
  Printf.printf "Day 4 Part 2 --> %d\n" out_2
;;
