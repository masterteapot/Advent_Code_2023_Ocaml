open Base

(* open Core *)
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

type instructions =
  { dir : direction
  ; count : int
  ; color_code : string
  ; c : string
  }

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

let print_instructions i =
  printf
    "{ dir: %s; count: %d; color_code: %s; c: %s }\n"
    (match i.dir with
     | N -> "N"
     | W -> "W"
     | S -> "S"
     | E -> "E")
    i.count
    i.color_code
    i.c
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
  let rec aux x y dir count acc =
    if count = 0
    then acc
    else (
      match dir with
      | N -> aux x (Int.pred y) dir (Int.pred count) ((x, Int.pred y) :: acc)
      | E -> aux (Int.succ x) y dir (Int.pred count) ((Int.succ x, y) :: acc)
      | S -> aux x (Int.succ y) dir (Int.pred count) ((x, Int.succ y) :: acc)
      | W -> aux (Int.pred x) y dir (Int.pred count) ((Int.pred x, y) :: acc))
  in
  aux (fst cur) (snd cur) i.dir i.count acc
;;

let get_option x =
  match x with
  | Some x -> x
  | None -> failwith "Missing option"
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

let move loc dir =
  let x = fst loc in
  let y = snd loc in
  match dir with
  | N -> x, Int.pred y
  | E -> Int.succ x, y
  | W -> Int.pred x, y
  | S -> x, Int.succ y
;;

let out_of_bounds ~min_x ~max_x ~min_y ~max_y ~x ~y =
  x < min_x || x > max_x || y < min_y || y > max_y
;;

let am_i_trench ~hg =
  let keys = Hashtbl.keys hg in
  let rec get_accs acc_lvl acc loc =
    match acc_lvl with
    | NotTrench -> NotTrench, acc
    | InTrench -> InTrench, acc
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
         let level, acc = get_accs acc_lvl (loc :: acc) (move loc N) in
         let level, acc = get_accs level acc (move loc E) in
         let level, acc = get_accs level acc (move loc S) in
         let level, acc = get_accs level acc (move loc W) in
         if Stdlib.( = ) level Unknown then InTrench, acc else level, acc)
  in
  let aux loc =
    let level = Hashtbl.find hg loc in
    match level with
    | None -> ()
    | Some NotTrench -> ()
    | Some Trench -> ()
    | Some InTrench -> ()
    | Some Unknown ->
      let level, acc = get_accs Unknown [] loc in
      update_grounds ~hash_grounds:hg ~new_grounds:acc ~level
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
    then aux 0 (y + 1) [] (List.rev sub_acc :: main_acc)
    else aux (x + 1) y (Hashtbl.find h (x, y) :: sub_acc) main_acc
  in
  aux min_x min_y [] []
;;

let input =
  read_lines "inputs/18.txt" |> remove_empty_string |> List.map ~f:map_instructions
;;

let trenches = List.fold_left input ~init:[ 0, 0 ] ~f:trench_walker
let grounds = make_grounds trenches

let h =
  Base.Hashtbl.create_mapped
    (module struct
      type t = int * int [@@deriving sexp, compare, hash]
    end)
    ~get_key:(fun x -> x)
    ~get_data:(fun _ -> Unknown)
    grounds
;;

let h =
  match h with
  | `Ok x -> x
  | _ -> failwith "why duplicates?"
;;

let () = update_grounds ~hash_grounds:h ~new_grounds:trenches ~level:Trench
let () = am_i_trench ~hg:h
let ls_of_hg = list_of_grounds h

let trench_string =
  List.map ls_of_hg 
    ~f:(fun x ->
    List.fold_left x ~init:""
      ~f:(fun acc y ->
          (match y with
           | Some Trench -> acc ^ "# "
           | Some InTrench -> acc ^  "I "
           | Some NotTrench -> acc ^ ". "
           | _ -> acc ^ "U "))
      )
;;

let out = Stdio.Out_channel.create "trenches.txt"
let () = Stdio.Out_channel.output_lines out trench_string
let () = Stdio.Out_channel.close out

let () = Stdlib.print_newline ()

(* Part 1 *)
let part_one () =
  let out_1 = count_trenches ~hash_grounds:h in
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
