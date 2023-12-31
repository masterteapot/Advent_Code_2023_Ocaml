open Base
open Utilities
open Stdlib.Printf

type direction =
  | N
  | E
  | S
  | W

type level =
  | Trench
  | InTrench
  | NotTrench
  | Unknown

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
  }

let print_corner c =
  printf
    "at: (%d, %d); prev: (%d, %d); next: (%d, %d)\n"
    (fst c.at)
    (snd c.at)
    (fst c.prev)
    (snd c.prev)
    (fst c.next)
    (snd c.next)
;;

let move ~loc ~dir ~num =
  let x = fst loc in
  let y = snd loc in
  match dir with
  | N -> x, y - num
  | E -> x + num, y
  | W -> x - num, y
  | S -> x, y + num
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

let print_hash_grounds ~key:(x, y) ~data:g =
  let g =
    match g with
    | Trench -> "Trench"
    | InTrench -> "InTrench"
    | NotTrench -> "NotTrench"
    | Unknown -> "Unknown"
  in
  printf "{ %d, %d -> %s }\n" x y g
;;

let trench_walker acc i =
  let cur =
    match List.hd acc with
    | Some x -> x
    | None -> failwith "List is empty"
  in
  let rec aux loc dir count acc =
    if count = 0
    then acc
    else (
      let new_loc = move ~loc ~dir ~num:1 in
      aux new_loc dir (Int.pred count) (new_loc :: acc))
  in
  aux cur i.dir i.count acc
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
      Hashtbl.set cm ~key:loc ~data:{ at = loc; next = new_loc; prev = List.hd_exn acc };
      aux new_loc (loc :: acc) tl
  in
  aux (0, 0) [ 0, 0 ] ls;
  cm
;;

let make_grounds trenches =
  let min_x =
    List.min_elt (List.map ~f:fst trenches) ~compare:Base.compare |> get_option
  in
  let max_x =
    List.max_elt (List.map ~f:fst trenches) ~compare:Base.compare |> get_option
  in
  let min_y =
    List.min_elt (List.map ~f:snd trenches) ~compare:Base.compare |> get_option
  in
  let max_y =
    List.max_elt (List.map ~f:snd trenches) ~compare:Base.compare |> get_option
  in
  let rec aux x y acc =
    match x with
    | _ when x > max_x -> acc
    | x ->
      (match y with
       | _ when y > max_y -> aux (x + 1) min_y acc
       | y -> aux x (y + 1) ((x, y) :: acc))
  in
  aux min_x min_y []
;;

let update_grounds ~hash_grounds:h ~new_grounds:ls ~level:lv =
  if List.is_empty ls then ();
  let rec aux = function
    | [] -> ()
    | hd :: tl ->
      Hashtbl.update h hd ~f:(fun x ->
        match x with
        | Some _ -> lv
        | _ -> Unknown);
      aux tl
  in
  aux ls
;;

let out_of_bounds ~min_x ~max_x ~min_y ~max_y ~x ~y =
  x < min_x || x > max_x || y < min_y || y > max_y
;;

let am_i_trench ~hg =
  let keys = Hashtbl.keys hg in
  let rec get_accs acc_lvl acc loc =
    match acc_lvl with
    | NotTrench -> NotTrench, acc
    | _ ->
      let level = Hashtbl.find hg loc in
      (match level with
       | _ when List.mem acc loc ~equal:(fun (a, b) (x, y) -> a = x && b = y) ->
         acc_lvl, acc
       | None -> NotTrench, acc
       | Some NotTrench -> NotTrench, acc
       | Some Trench -> Unknown, acc
       | Some InTrench -> InTrench, acc
       | Some Unknown ->
         let level, acc = get_accs acc_lvl (loc :: acc) (move ~loc ~dir:N ~num:1) in
         let level, acc = get_accs level acc (move ~loc ~dir:E ~num:1) in
         let level, acc = get_accs level acc (move ~loc ~dir:S ~num:1) in
         let level, acc = get_accs level acc (move ~loc ~dir:W ~num:1) in
         if Stdlib.( = ) level Unknown then InTrench, acc else level, acc)
  in
  let aux loc =
    let level = Hashtbl.find hg loc in
    match level with
    | Some Unknown ->
      let level, acc = get_accs Unknown [] loc in
      update_grounds ~hash_grounds:hg ~new_grounds:acc ~level
    | _ -> ()
  in
  List.iter keys ~f:aux
;;

let count_trenches ~hash_grounds:hg =
  Base.Hashtbl.fold hg ~init:0 ~f:(fun ~key:_ ~data:d acc ->
    match d with
    | Trench -> acc + 1
    | InTrench -> acc + 1
    | _ -> acc)
;;

let count_hash_ground_type ~hash_grounds:hg ~(level : level) =
  Base.Hashtbl.fold hg ~init:0 ~f:(fun ~key:_ ~data:(d : level) acc ->
    if Stdlib.( = ) d level then acc + 1 else acc)
;;

let list_of_grounds h =
  let grounds = Hashtbl.keys h in
  let min_x =
    List.min_elt (List.map ~f:fst grounds) ~compare:Base.compare |> get_option
  in
  let max_x =
    List.max_elt (List.map ~f:fst grounds) ~compare:Base.compare |> get_option
  in
  let min_y =
    List.min_elt (List.map ~f:snd grounds) ~compare:Base.compare |> get_option
  in
  let max_y =
    List.max_elt (List.map ~f:snd grounds) ~compare:Base.compare |> get_option
  in
  let rec aux x y sub_acc main_acc =
    if y > max_y
    then List.rev (List.rev sub_acc :: main_acc)
    else if x > max_x
    then aux min_x (y + 1) [] (List.rev sub_acc :: main_acc)
    else aux (x + 1) y (Hashtbl.find h (x, y) :: sub_acc) main_acc
  in
  aux min_x min_y [] []
;;

let input =
  read_lines "inputs/18_m4.txt" |> remove_empty_string |> List.map ~f:map_instructions
;;

let trenches = List.fold_left input ~init:[ 0, 0 ] ~f:trench_walker
let grounds = make_grounds trenches

let hg =
  Base.Hashtbl.create_mapped
    (module struct
      type t = int * int [@@deriving sexp, compare, hash]
    end)
    ~get_key:(fun x -> x)
    ~get_data:(fun _ -> Unknown)
    grounds
  |> function
  | `Ok x -> x
  | _ -> failwith "why duplicates?"
;;

let () = update_grounds ~hash_grounds:hg ~new_grounds:trenches ~level:Trench
let () = am_i_trench ~hg
let ls_of_hg = list_of_grounds hg

let trench_string =
  List.map ls_of_hg ~f:(fun x ->
    List.fold_left x ~init:"" ~f:(fun acc y ->
      match y with
      | Some Trench -> acc ^ "# "
      | Some InTrench -> acc ^ "+ "
      | Some NotTrench -> acc ^ ". "
      | _ -> acc ^ "U "))
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

let sort_locs_by_height x y = if snd x > snd y then 1 else if snd x = snd y then 0 else -1

let counting_corners cm =
  let get_active_corners x =
    Hashtbl.filter cm ~f:(fun c -> fst c.at <= x && (fst c.next >= x || fst c.prev >= x))
    |> Hashtbl.data
    |> List.sort ~compare:(fun x y -> sort_locs_by_height x.at y.at)
  in
  let rec calc_height is_trench height ls =
    match ls with
    | [] -> height
    | hd :: md :: tl when is_trench ->
      let new_height = abs (snd hd.at - snd md.at) + 1 in
      calc_height false (height + new_height) (md :: tl)
    | _ :: md :: tl -> calc_height true height (md :: tl)
    | _ -> height
  in
  let calc_size last_x cur_x =
    let width = abs (cur_x - last_x) in
    let height = calc_height true 0 (get_active_corners last_x) in
    Stdlib.print_newline ();
    printf "W: %d; H: %d; Area: %d\n" width height (width * height);
    width * height
  in
  let sorted_corners =
    List.sort (Hashtbl.data cm) ~compare:(fun x y -> sort_locs x.at y.at)
  in
  let rec aux last_x cur_x acc size sc =
    match sc with
    | [] -> calc_size last_x cur_x + size
    | hd :: tl when fst hd.at = cur_x -> aux last_x cur_x (hd :: acc) size tl
    | hd :: tl when fst hd.at > cur_x ->
      aux cur_x (fst hd.at) [ hd ] (calc_size last_x cur_x + size) tl
    | _ -> failwith "somehow we are not in left to right order"
  in
  let min_x = fst (List.hd_exn sorted_corners).at in
  aux min_x min_x [] (calc_height true 0 (get_active_corners min_x)) sorted_corners
;;

let cm = corners_of_instructions input
let out_2 = counting_corners cm
let () = Stdlib.print_newline ()
let () = Stdlib.print_newline ()
let () = List.iter trench_string ~f:Stdlib.print_endline
let () = Stdlib.print_newline ()
let () = printf "%d\n" out_2

(* Part 1 *)
let part_one () =
  let out_1 = count_trenches ~hash_grounds:hg in
  printf "Day 18 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let out_2 = 2 in
  printf "Day 18 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
