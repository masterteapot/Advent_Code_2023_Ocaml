open Base
open Utilities
open Stdlib.Printf

type direction =
  | N
  | E
  | S
  | W

type angle =
  | BR
  | TR
  | BL
  | TL

type instruction =
  { dir : direction
  ; count : int
  ; color_code : string
  ; c : string
  }

type corner =
  { at : int * int
  ; next : int * int
  ; prev : int * int
  ; angle : angle
  }

let move ~loc ~dir ~num =
  let x = fst loc in
  let y = snd loc in
  match dir with
  | N -> x, y - num
  | E -> x + num, y
  | W -> x - num, y
  | S -> x, y + num
;;

let get_angle at next prev =
  let hc = if snd at = snd next then next else prev in
  let vc = if fst at = fst next then next else prev in
  let hd = if fst hc > fst at then E else W in
  let vd = if snd vc > snd at then S else N in
  match vd, hd with
  | N, W -> BR
  | S, W -> TR
  | N, E -> BL
  | S, E -> TL
  | _ -> failwith "Not possible corner combination"
;;

let map_instructions i =
  let groups = String.split ~on:' ' i in
  assert (List.length groups = 3);
  let rec aux acc = function
    | '(' :: tl -> aux acc tl
    | ')' :: _ -> String.of_list @@ List.rev acc
    | hd :: tl -> aux (hd :: acc) tl
    | [] -> failwith "We should have already exited"
  in
  let color_code =
    aux
      []
      (String.to_list
       @@
       match List.nth groups 2 with
       | Some x -> x
       | None -> failwith "missing string")
  in
  let dir =
    match List.nth groups 0 with
    | Some "R" -> E
    | Some "D" -> S
    | Some "L" -> W
    | Some "U" -> N
    | _ -> failwith "unexpected instruction string"
  in
  let count =
    Stdlib.int_of_string
    @@
    match List.nth groups 1 with
    | Some x -> x
    | None -> failwith "i dunno"
  in
  { dir
  ; count
  ; color_code
  ; c =
      (match List.nth groups 0 with
       | Some x -> x
       | None -> failwith "i dunno 2")
  }
;;

let get_option x =
  match x with
  | Some x -> x
  | None -> failwith "Missing option"
;;

let corners_of_instructions (ls : instruction list) =
  let cm =
    Base.Hashtbl.create
      ~growth_allowed:true
      ~size:(List.length ls)
      (module struct
        type t = int * int [@@deriving sexp, compare, hash]
      end)
  in
  let start = 0, 0 in
  let rec aux loc acc = function
    | [] ->
      Hashtbl.update cm start ~f:(fun x ->
        match x with
        | Some x -> { x with prev = List.hd_exn acc }
        | None -> failwith "Couldn't find starting corner")
    | hd :: tl ->
      let new_loc = move ~loc ~dir:hd.dir ~num:hd.count in
      let c_angle = get_angle loc new_loc (List.hd_exn acc) in
      Hashtbl.set
        cm
        ~key:loc
        ~data:{ at = loc; next = new_loc; prev = List.hd_exn acc; angle = c_angle };
      aux new_loc (loc :: acc) tl
  in
  aux (0, 0) [ 0, 0 ] ls;
  Base.Hashtbl.update cm (0, 0) ~f:(fun x ->
    let x = get_option x in
    let new_angle = get_angle x.at x.next x.prev in
    { x with angle = new_angle });
  cm
;;

let sort_locs x y =
  if fst x > fst y
  then 1
  else if fst x = fst y && snd x > snd y
  then 1
  else if fst x = fst y && snd x = snd y
  then 0
  else -1
;;

let equal_corner c1 c2 = fst c1 = fst c2 && snd c1 = snd c2
let sort_locs_by_height x y = if snd x > snd y then 1 else if snd x = snd y then 0 else -1
let is_connected_corners c1 c2 = equal_corner c1.next c2.at || equal_corner c1.prev c2.at

let string_of_corner_angle c =
  match c.angle with
  | BR -> "BR"
  | TR -> "TR"
  | BL -> "BL"
  | TL -> "TL"
;;

let counting_corners cm =
  let get_active_corners x =
    Hashtbl.filter cm ~f:(fun c ->
      fst c.at = x || (fst c.at < x && (fst c.next > x || fst c.prev > x)))
    |> Hashtbl.data
    |> List.sort ~compare:(fun x y -> sort_locs_by_height x.at y.at)
  in
  let is_closed_corner = function
    | BR -> true
    | TR -> true
    | _ -> false
  in
  let rec calc_height x_val was_trench height height_loss ls =
    Stdlib.print_newline ();
    List.iter ls ~f:(fun x ->
      printf " (%d, %d) [%s]; " (fst x.at) (snd x.at) (string_of_corner_angle x));
    printf "\n%d -> " height;
    match ls with
    | [] -> height, height_loss
    | [ hd; md ]
    (* If you are at the last two connections, you are in trench and the top is not closed but the bottom is then you add to the missing height but not to height *)
      when was_trench
           && (not (is_closed_corner hd.angle))
           && is_closed_corner md.angle
           && is_connected_corners hd md
           && fst hd.at = x_val
           && fst md.at = x_val ->
      let new_height_loss = abs (snd hd.at - snd md.at) + height_loss in
      printf "%d" height;
      printf " via %d; height_loss: %d\n" 1 height_loss;
      calc_height x_val false height new_height_loss [ md ]
    | [ hd; md ]
    (* if you are in trench and the first is closed and it is the last two elements and the second is open then add to height *)
      when was_trench
           && is_closed_corner hd.angle
           && (not (is_closed_corner md.angle))
           && is_connected_corners hd md
           && fst hd.at = x_val
           && fst md.at = x_val ->
      let new_height = abs (snd hd.at - snd md.at) + height in
      printf "%d" height;
      printf " via %d; height_loss: %d\n" 2 height_loss;
      calc_height x_val true new_height height_loss [ md ]
    | hd :: md :: tl
    (* When we have a double closing connected connection and we were in trench then we stay in trench *)
      when was_trench
           && is_closed_corner hd.angle
           && is_closed_corner md.angle
           && is_connected_corners hd md
           && fst hd.at = x_val
           && fst md.at = x_val ->
      let new_height = abs (snd hd.at - snd md.at) - 1 + height in
      printf "%d" new_height;
      printf " via %d; height_loss: %d\n" 3 height_loss;
      calc_height x_val true new_height height_loss (md :: tl)
    | hd :: md :: tl
    (* When we have a double closing UNconnected connection and we were in trench then we stay in trench *)
      when was_trench
           && is_closed_corner hd.angle
           && is_closed_corner md.angle
           && (not @@ is_connected_corners hd md)
           && fst hd.at = x_val
           && fst md.at = x_val ->
      let new_height = abs (snd hd.at - snd md.at) + height in
      printf "%d" new_height;
      printf " via %d; height_loss: %d\n" 4 height_loss;
      calc_height x_val true new_height height_loss (md :: tl)
    | hd :: md :: tl
    (* When you are in trench and only the top is closed, and it is connected then you add to the height and stay in trench *)
      when was_trench
           && is_closed_corner hd.angle
           && (not @@ is_closed_corner md.angle)
           && is_connected_corners hd md
           && fst hd.at = x_val
           && fst md.at = x_val ->
      let new_height = abs (snd hd.at - snd md.at) + height in
      printf "%d" new_height;
      printf " via %d; height_loss: %d\n" 5 height_loss;
      calc_height x_val true new_height height_loss (md :: tl)
    | hd :: md :: tl
    (* When we are not in trench and have two closed corners on the same plane but they are not connected then you are not in trench and you do not add their height *)
      when is_closed_corner hd.angle
           && is_closed_corner md.angle
           && (not @@ is_closed_corner md.angle)
           && fst hd.at = x_val
           && fst md.at = x_val ->
      let new_height = height in
      printf "%d" new_height;
      printf " via %d; height_loss: %d\n" 6 height_loss;
      calc_height x_val false new_height height_loss (md :: tl)
    | hd :: md :: tl
    (* When we are in trench and have only the top closed corner and you are not connected with the bottom, and it is not on the same x_axis then you are still in trench *)
      when was_trench
           && is_closed_corner hd.angle
           && (not @@ is_closed_corner md.angle)
           && (not (is_connected_corners hd md))
           && fst hd.at = x_val
           && snd md.at <> x_val ->
      let new_height = abs (snd hd.at - snd md.at) + 1 + height in
      printf "%d" new_height;
      printf " via %d; height_loss: %d\n" 7 height_loss;
      calc_height x_val true new_height height_loss (md :: tl)
    | hd :: md :: tl
    (*  When the top is closed and not connected to the bottom and it is not on the same x axis and you were not in trench then you stay out of trench
    *)
      when (not was_trench)
           && is_closed_corner hd.angle
           && fst hd.at = x_val
           && snd md.at <> x_val
           && not (is_connected_corners hd md) ->
      let new_height = height in
      printf "%d" new_height;
      printf " via %d; height_loss: %d\n" 8 height_loss;
      calc_height x_val false new_height height_loss (md :: tl)
    | hd :: md :: tl
    (* When you are connected and closed corners but not in trench then we add your height to the height loss and stay out of trench *)
      when (not was_trench)
           && is_closed_corner hd.angle
           && is_closed_corner md.angle
           && is_connected_corners hd md
           && fst hd.at = x_val
           && fst md.at = x_val ->
      let new_height_loss = abs (snd hd.at - snd md.at) + 1 + height_loss in
      printf "%d" height;
      printf " via %d; height_loss: %d\n" 9 new_height_loss;
      calc_height x_val false height new_height_loss (md :: tl)
    | hd :: md :: tl
    (* When you are not in trench and your top is a closed corner but the bottom isn't but they are connected then you are out of trench but you can add some to the lost height value *)
      when (not was_trench)
           && is_closed_corner hd.angle
           && is_connected_corners hd md
           && fst hd.at = x_val
           && fst md.at = x_val ->
      let new_height_loss = abs (snd hd.at - snd md.at) + height_loss in
      printf "%d" height;
      printf " via %d; height_loss: %d\n" 10 new_height_loss;
      calc_height x_val false height new_height_loss (md :: tl)
      (* if last two elements and first isn't closed but bottom is then add to missing height *)
    | hd :: md :: tl when was_trench && is_connected_corners hd md ->
      printf "%d" height;
      printf " via %d; height_loss: %d\n" 11 height_loss;
      calc_height
        x_val
        false
        height
        (abs (snd hd.at - snd md.at) + height_loss - 1)
        (md :: tl)
    | _ :: md :: tl when was_trench ->
      printf "%d" height;
      printf " via %d; height_loss: %d\n" 12 height_loss;
      calc_height x_val false height height_loss (md :: tl)
    | hd :: md :: tl ->
      let new_height = abs (snd hd.at - snd md.at) + 1 + height in
      printf "%d" new_height;
      printf " via %d; height_loss: %d\n" 13 height_loss;
      calc_height x_val true new_height height_loss (md :: tl)
    | _ ->
      printf "\n\n\nAt the end with HL: %d\n" height_loss;
      height, height_loss
  in
  let sorted_corners =
    List.sort (Hashtbl.data cm) ~compare:(fun x y -> sort_locs x.at y.at)
  in
  let rec aux last_x cur_x acc last_height size sc =
    Stdlib.print_newline ();
    match sc with
    | [] ->
      let width = abs (cur_x - last_x) + 1 in
      let height, height_difference =
        calc_height last_x false 0 0 (get_active_corners last_x)
      in
      Stdlib.print_newline ();
      printf
        "\n-- W: %d; H: %d; Area: %d --\n"
        width
        height
        ((width * height) + height_difference);
      (width * height) + height_difference + size
    | hd :: tl when fst hd.at = cur_x -> aux last_x cur_x (hd :: acc) last_height size tl
    | hd :: tl when fst hd.at > cur_x ->
      let width = abs (cur_x - last_x) in
      let height, height_difference =
        calc_height last_x false 0 0 (get_active_corners last_x)
      in
      Stdlib.print_newline ();
      printf
        "\n-- W: %d; H: %d; Area: %d --\n"
        width
        height
        ((width * height) + height_difference);
      let new_size = (width * height) + height_difference in
      aux cur_x (fst hd.at) [ hd ] height (size + new_size) tl
    | _ -> failwith "somehow we are not in left to right order"
  in
  let min_x = fst (List.hd_exn sorted_corners).at in
  aux min_x min_x [] 0 0 sorted_corners
;;

let input = "inputs/18_t2.txt"
let instructions = read_lines input |> remove_empty_string |> List.map ~f:map_instructions
let () = Stdlib.print_newline ()
let cm = corners_of_instructions instructions
let out_2 = counting_corners cm
let () = Stdlib.print_newline ()
let () = Stdlib.print_newline ()
let () = printf "%d\n" out_2

(* Part 1 *)
let part_one () =
  let open Day_18_p1 in
  part_one input
;;

(* Part 2 *)
let part_two () =
  let out_2 = out_2 in
  printf "Day 18 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
