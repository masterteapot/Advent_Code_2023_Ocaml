open Batteries
open Utilities

type category =
  | O
  | I
  | P
  | X

type direction =
  | L
  | R
  | U
  | D
  | X

type pipe =
  { in_loc : int * int
  ; at_loc : int * int
  ; out_loc : int * int
  ; mutable distance : int
  }

type spot =
  { char : char
  ; mutable category : category
  ; location : int * int
  }

type past_spot =
  { mutable top : bool
  ; mutable right : bool
  ; mutable bottom : bool
  ; mutable left : bool
  }

let print_pipe_array pipe_array =
  let max_x = Array.length pipe_array.(0) in
  let x_axis = Array.make max_x 0 in
  print_newline ();
  print_string "   ";
  Array.iteri (fun n _ -> Printf.printf " %d " (Int.rem n 10)) x_axis;
  print_newline ();
  Array.iteri
    (fun i x ->
      print_newline ();
      Printf.printf "%d  " (Int.rem i 10);
      Array.iter
        (fun y ->
          let s = if y.category <> P then "." else String.make 1 y.char in
          Printf.printf " %s " s)
        x)
    pipe_array;
  print_newline ()
;;

let print_pipe p =
  Printf.printf
    "{in_loc: (%d, %d) ; at_loc: (%d, %d) ; out_loc: (%d, %d) ; distance: %d}\n"
    (fst p.in_loc)
    (snd p.in_loc)
    (fst p.at_loc)
    (snd p.at_loc)
    (fst p.out_loc)
    (snd p.out_loc)
    p.distance
;;

let find_start_directions ll s_pos =
  let x_min = 0 in
  let y_min = 0 in
  let x_max = List.length @@ List.hd ll in
  let y_max = List.length ll in
  let surr_pos =
    match s_pos with
    | x, y -> [ R, x + 1, y; L, x - 1, y; U, x, y - 1; D, x, y + 1 ]
  in
  let rec get_surr_chars surr_pos acc =
    match surr_pos with
    | (_, x, y) :: tl when x < x_min || x > x_max || y < y_min || y > y_max ->
      get_surr_chars tl acc
    | (dir, x, y) :: tl ->
      let surr_char = get_llc ll (x, y) in
      (match dir, surr_char with
       | U, '|' -> get_surr_chars tl ((x, y) :: acc)
       | D, '|' -> get_surr_chars tl ((x, y) :: acc)
       | L, '-' -> get_surr_chars tl ((x, y) :: acc)
       | R, '-' -> get_surr_chars tl ((x, y) :: acc)
       | L, 'L' -> get_surr_chars tl ((x, y) :: acc)
       | D, 'L' -> get_surr_chars tl ((x, y) :: acc)
       | R, 'J' -> get_surr_chars tl ((x, y) :: acc)
       | D, 'J' -> get_surr_chars tl ((x, y) :: acc)
       | R, '7' -> get_surr_chars tl ((x, y) :: acc)
       | U, '7' -> get_surr_chars tl ((x, y) :: acc)
       | L, 'F' -> get_surr_chars tl ((x, y) :: acc)
       | U, 'F' -> get_surr_chars tl ((x, y) :: acc)
       | _ -> get_surr_chars tl acc)
    | [] -> acc
  in
  get_surr_chars surr_pos []
;;

let find_out_directions ll s_pos =
  let x, y =
    match s_pos with
    | x, y -> x, y
  in
  let cur_char = get_llc ll s_pos in
  match cur_char with
  | '|' -> (x, y - 1), (x, y + 1)
  | '-' -> (x + 1, y), (x - 1, y)
  | 'L' -> (x, y - 1), (x + 1, y)
  | 'J' -> (x, y - 1), (x - 1, y)
  | '7' -> (x - 1, y), (x, y + 1)
  | 'F' -> (x, y + 1), (x + 1, y)
  | _ -> failwith ("'" ^ String.make 1 cur_char ^ "' is not a valid character")
;;

let walk_the_pipes start_pos forward_move backwards_move ll =
  let rec aux in_loc at_loc counter acc =
    if at_loc = start_pos
    then acc
    else (
      let out_options = find_out_directions ll at_loc in
      let out_loc =
        match out_options with
        | x, y when y = in_loc -> x
        | x, y when x = in_loc -> y
        | x, y ->
          let f_char = get_llc ll at_loc in
          Printf.printf
            "\n\
             -%s | at_loc: (%d, %d) | in_loc: (%d, %d) | first_opt: (%d, %d) | \
             second_opt: (%d, %d)\n"
            (String.make 1 f_char)
            (fst at_loc)
            (snd at_loc)
            (fst in_loc)
            (snd in_loc)
            (fst x)
            (snd x)
            (fst y)
            (snd y);
          failwith "we should only have 1 possible out"
      in
      let new_pipe = { in_loc; at_loc; out_loc; distance = counter + 1 } in
      aux at_loc out_loc (counter + 1) (new_pipe :: acc))
  in
  let at_loc = start_pos in
  let out_loc = forward_move in
  let in_loc = backwards_move in
  let distance = 0 in
  aux start_pos forward_move 0 [ { in_loc; at_loc; out_loc; distance } ]
;;

let make_pipe_array input =
  let char_array =
    List.map List.to_seq input |> List.map Array.of_seq |> List.to_seq |> Array.of_seq
  in
  Array.mapi
    (fun y arr -> Array.mapi (fun x c -> { char = c; location = x, y; category = X }) arr)
    char_array
;;

let can_escape max_x max_y location pipe_array =
  let is_possible_direction location ht =
    let x, y = fst location, snd location in
    x <= max_x
    && y <= max_y
    && x >= 0
    && y >= 0
    && ((not (Hashtbl.mem ht (x, y)))
        ||
        let pm = Hashtbl.find ht (x, y) in
        let is_exhasted = pm.top && pm.right && pm.bottom && pm.left in
        not is_exhasted)
    && pipe_array.(y).(x).category <> P
  in
  let rec aux location from_direction past_spots =
    let x, y = fst location, snd location in
    x <> 0
    && y <> 0
    && x <> max_x
    && y <> max_y
    &&
    (if not (Hashtbl.mem past_spots location)
     then (
       let v =
         match from_direction with
         | X -> { top = false; bottom = false; right = false; left = false }
         | U -> { top = true; bottom = false; right = false; left = false }
         | R -> { top = false; bottom = false; right = true; left = false }
         | D -> { top = false; bottom = true; right = false; left = false }
         | L -> { top = false; bottom = false; right = false; left = true }
       in
       Hashtbl.add past_spots location v);
     let () =
       match from_direction with
       | U -> (Hashtbl.find past_spots location).top <- true
       | R -> (Hashtbl.find past_spots location).right <- true
       | D -> (Hashtbl.find past_spots location).bottom <- true
       | L -> (Hashtbl.find past_spots location).left <- true
       | X -> ()
     in
     let pm = Hashtbl.find past_spots location in
     (* print_td location; *)
     (* Printf.printf "%b %b %b %b\n" pm.top pm.right pm.bottom pm.left; *)
     let dt = x, y - 1 in
     let dr = x + 1, y in
     let db = x, y + 1 in
     let dl = x - 1, y in
     let can_top = is_possible_direction dt past_spots in
     if not can_top then (Hashtbl.find past_spots location).top <- true;
     let can_right = is_possible_direction dr past_spots in
     if not can_right then (Hashtbl.find past_spots location).right <- true;
     let can_bottom = is_possible_direction db past_spots in
     if not can_bottom then (Hashtbl.find past_spots location).bottom <- true;
     let can_left = is_possible_direction dl past_spots in
     if not can_left then (Hashtbl.find past_spots location).left <- true;
     match
       ( can_top && not pm.top
       , can_right && not pm.right
       , can_bottom && not pm.bottom
       , can_left && not pm.left )
     with
     | true, _, _, _ ->
       (Hashtbl.find past_spots location).top <- true;
       aux dt D past_spots
     | _, true, _, _ ->
       (Hashtbl.find past_spots location).right <- true;
       aux dr L past_spots
     | _, _, true, _ ->
       (Hashtbl.find past_spots location).bottom <- true;
       aux db U past_spots
     | _, _, _, true ->
       (Hashtbl.find past_spots location).left <- true;
       aux dl R past_spots
     | _, _, _, _
       when Hashtbl.for_all (fun _ v -> v.top && v.right && v.bottom && v.left) past_spots
       -> true
     | _, _, _, _ ->
       let rt =
         Hashtbl.filteri
           (fun _ v ->
             v.top = false || v.right = false || v.bottom = false || v.left = false)
           past_spots
       in
       let ks = Hashtbl.keys rt in
       (* Printf.printf "%d\n" (Enum.count ks); *)
       let ns = Enum.peek ks in
       (match ns with
        | Some new_loc -> aux new_loc X past_spots
        | None -> failwith "we shouldn't be here"))
  in
  pipe_array.(snd location).(fst location).category <> P
  && fst location <> 0
  && snd location <> 0
  && fst location <> max_x
  && snd location <> max_y
  &&
  let ht = Hashtbl.create (max_x * max_y) in
  Hashtbl.add ht location { top = false; right = false; bottom = false; left = false };
  aux location X ht
;;

let find_innerds pipe_array =
  let max_x = Array.length pipe_array.(0) - 1 in
  let max_y = Array.length pipe_array - 1 in
  Array.map
    (fun x ->
      Array.fold_left
        (fun acc y ->
          if can_escape max_x max_y y.location pipe_array
          then
            (* Printf.printf "\n(%d, %d)\n" (fst y.location) (snd y.location); *)
            acc + 1
          else acc)
        0
        x)
    pipe_array
  |> Array.fold_left ( + ) 0
;;

(* let () = *)
(*   List.iter *)
(*     (fun p -> *)
(*       let x = fst p.at_loc in *)
(*       let y = snd p.at_loc in *)
(*       pipe_array.(y).(x).category <- P) *)
(*     pipes *)
(* ;; *)
(*  *)
(* let counts = find_innerds pipe_array *)
(* let () = print_newline () *)

(* let () = print_newline () *)
(* let () = print_pipe_array pipe_array *)
let () = print_newline ()

(* let () = *)
(*   Printf.printf *)
(*     "\nTable size = %d\nNumber of pipes are: %d\nTrapped area: %d\n" *)
(*     (Array.length pipe_array * Array.length pipe_array.(0)) *)
(*     (List.length pipes) *)
(*     counts *)
(* ;; *)
(*  *)
(* let max_x = Array.length pipe_array.(0) - 1 *)
(* let max_y = Array.length pipe_array - 1 *)

(* Part 1 *)
let part_one () =
  let input =
    Utilities.read_lines "inputs/10.txt"
    |> Utilities.remove_empty_string
    |> List.map String.explode
  in
  (* let is_pipe loc pipes = List.exists (fun x -> x.at_loc = loc) pipes in *)
  print_newline ();
  let start = find_llc input 'S' in
  let start_move_options = find_start_directions input start in
  assert (List.length start_move_options = 2);
  let forward_move = List.at start_move_options 0 in
  let backwards_move = List.at start_move_options 1 in
  let pipes = walk_the_pipes start forward_move backwards_move input in
  let distances = List.map (fun x -> x.distance) pipes in
  let max_distance = List.first distances in
  let med_index = max_distance / 2 in
  let longest_distance = List.at distances med_index in
  let out_1 = longest_distance in
  Printf.printf "Day 10 Part 1 --> %d\n" out_1
;;

(* (* Part 2 *) *)
(* let part_two () = *)
(* let pipe_array = make_pipe_array input in *)
(*   let out_2 = counts in *)
(*   Printf.printf "Day 10 Part 2 --> %d\n" out_2 *)
(* ;; *)

let main () = part_one ()
(* part_two () *)
