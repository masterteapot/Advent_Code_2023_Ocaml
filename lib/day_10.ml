open Batteries

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

let print_td td = Printf.printf "(%d, %d)\n" (fst td) (snd td)

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

let find_char ll char =
  let rec aux ll in_count out_count =
    match ll with
    | [] -> failwith ("char ['" ^ String.make 1 char ^ "'] not found")
    | in_list :: tl ->
      (match in_list with
       | [] -> aux tl 0 (out_count + 1)
       | hd :: _ when hd = char -> in_count, out_count
       | _ :: in_tl -> aux (in_tl :: tl) (in_count + 1) out_count)
  in
  aux ll 0 0
;;

let print_lld a_list =
  print_newline ();
  List.iter
    (fun x ->
      print_newline ();
      List.iter (fun y -> Printf.printf "%d, " y) x;
      print_newline ())
    a_list;
  print_newline ()
;;

let get_ll_char ll pos =
  let in_list = List.at ll (snd pos) in
  List.at in_list (fst pos)
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
      let surr_char = get_ll_char ll (x, y) in
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
  let cur_char = get_ll_char ll s_pos in
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
          let f_char = get_ll_char ll at_loc in
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

let print_pipe_array pipe_array =
  Array.iter
    (fun x ->
      print_newline ();
      Array.iter
        (fun y ->
          let s = String.make 1 y.char in
          Printf.printf " %s " s)
        x)
    pipe_array;
  print_newline ()
;;

let make_pipe_array input =
  let char_array =
    List.map List.to_seq input |> List.map Array.of_seq |> List.to_seq |> Array.of_seq
  in
  Array.mapi
    (fun y arr -> Array.mapi (fun x c -> { char = c; location = x, y; category = X }) arr)
    char_array
;;

let print_llc a_list =
  print_newline ();
  List.iter
    (fun x ->
      print_newline ();
      List.iter (fun y -> Printf.printf "%c, " y) x;
      print_newline ())
    a_list;
  print_newline ()
;;

let input =
  Utilities.read_lines "inputs/10_t4.txt"
  |> Utilities.remove_empty_string
  |> List.map String.explode
;;

let is_pipe loc pipes = List.exists (fun x -> x.at_loc = loc) pipes
let () = print_newline ()
let start = find_char input 'S'
let start_move_options = find_start_directions input start
let () = assert (List.length start_move_options = 2)
let forward_move = List.at start_move_options 0
let backwards_move = List.at start_move_options 1
let pipes = walk_the_pipes start forward_move backwards_move input
let distances = List.map (fun x -> x.distance) pipes
let max_distance = List.first distances
let med_index = max_distance / 2
let longest_distance = List.at distances med_index
let pipe_array = make_pipe_array input

let () =
  List.iter
    (fun p ->
      let x = fst p.at_loc in
      let y = snd p.at_loc in
      pipe_array.(y).(x).category <- P)
    pipes
;;

let can_escape location pipe_array =
  let max_x = Array.length pipe_array.(0) in
  let max_y = Array.length pipe_array in
  let rec aux location past_spot =
    match location with
    | x, y ->
      let top_valid =
        y - 1 >= 0 && pipe_array.(y - 1).(x).category <> P && (x, y - 1) <> past_spot
      in
      let right_valid =
        x + 1 < max_x && pipe_array.(y).(x + 1).category <> P && (x + 1, y) <> past_spot
      in
      let bottom_valid =
        y + 1 < max_y && pipe_array.(y + 1).(x).category <> P && (x, y + 1) <> past_spot
      in
      let left_valid =
        x - 1 >= 0 && pipe_array.(y).(x - 1).category <> P && (x - 1, y) <> past_spot
      in
      Printf.printf "-- (%d, %d)\n" x y;
      pipe_array.(y).(x).category <> P
      && (x < 0
          || y < 0
          || x = max_x
          || y = max_y
          || (top_valid && aux (x, y - 1) (x, y))
          || (right_valid && aux (x + 1, y) (x, y))
          || (bottom_valid && aux (x, y + 1) (x, y))
          || (left_valid && aux (x - 1, y) (x, y)))
  in
  aux location location
;;

let find_innerds pipe_array =
  Array.map
    (fun x ->
      Array.fold_left
        (fun acc y -> if can_escape y.location pipe_array then acc + 1 else acc)
        0
        x)
    pipe_array
  |> Array.fold_left ( + ) 0
;;

let () = print_pipe_array pipe_array
let () = print_newline ()
let counts = find_innerds pipe_array
let () = print_newline ()
let () = print_int counts

(* let () = List.iter print_pipe pipes *)
(* let () = print_llc input *)
(* let () = print_newline () *)

(* Part 1 *)
let part_one () =
  let out_1 = longest_distance in
  Printf.printf "Day 10 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  Printf.printf "Day 10 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
