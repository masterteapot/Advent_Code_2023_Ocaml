open Batteries
open Utilities

type direction =
  | Up
  | Down
  | Right
  | Left

type mirror =
  | Blank
  | Horizontal
  | Vertical
  | TopRight
  | TopLeft

type space =
  { symbol : char
  ; mirr : mirror
  ; mutable energized : bool
  ; mutable count_up : int
  ; mutable count_down : int
  ; mutable count_left : int
  ; mutable count_right : int
  }

let print_mirrors mirrors =
  Array.iter
    (fun x ->
      print_newline ();
      Array.iter (fun y -> Printf.printf " %c " (if y.energized then '#' else y.symbol)) x)
    mirrors
;;

let make_mirror_array input =
  let mirrors =
    List.map List.to_seq input |> List.map Array.of_seq |> List.to_seq |> Array.of_seq
  in
  Array.map
    (fun x ->
      Array.map
        (fun y ->
          { symbol = y
          ; mirr =
              (match y with
               | '.' -> Blank
               | '/' -> TopRight
               | '\\' -> TopLeft
               | '-' -> Horizontal
               | '|' -> Vertical
               | _ -> failwith "unexpected char")
          ; energized = false
          ; count_up = 0
          ; count_down = 0
          ; count_left = 0
          ; count_right = 0
          })
        x)
    mirrors
;;

(* .|...\.... *)
(* |.-.\..... *)
(* .....|-... *)
(* ........|. *)
(* .......... *)
(* .........\ *)
(* ..../.\\.. *)
(* .-.-/..|.. *)
(* .|....-|.\ *)
(* ..//.|.... *)

(* ######.... *)
(* .#...#.... *)
(* .#...##### *)
(* .#...##... *)
(* .#...##... *)
(* .#...##... *)
(* .#..####.. *)
(* ########.. *)
(* .#######.. *)
(* .#...#.#.. *)
(* Ultimately, in this example, 46 tiles become energized. *)

let energize_mirrors mirrors =
  let max_y = Array.length mirrors in
  let max_x = Array.length mirrors.(0) in
  let rec aux x y dir =
    print_td (x, y);
    if x < 0 || x >= max_x || y < 0 || y >= max_y
    then ()
    else (
      let m = mirrors.(y).(x) in
      mirrors.(y).(x).energized <- true;
      if dir = Right && m.count_right > 0
      then ()
      else if dir = Left && m.count_left > 0
      then ()
      else if dir = Up && m.count_up > 0
      then ()
      else if dir = Down && m.count_down > 0
      then ();
      if dir = Right
      then mirrors.(y).(x).count_right <- 1
      else if dir = Left
      then mirrors.(y).(x).count_left <- 1
      else if dir = Up
      then mirrors.(y).(x).count_up <- 1
      else if dir = Down
      then mirrors.(y).(x).count_down <- 1;
      match dir with
      | Right when m.mirr = Vertical ->
        aux x (y + 1) Down;
        aux x (y - 1) Up
      | Right when m.mirr = TopRight -> aux x (y - 1) Up
      | Right when m.mirr = TopLeft -> aux x (y + 1) Down
      | Right -> aux (x + 1) y Right
      | Left when m.mirr = Vertical ->
        aux x (y + 1) Down;
        aux x (y - 1) Up
      | Left when m.mirr = TopLeft -> aux x (y - 1) Up
      | Left when m.mirr = TopRight -> aux x (y + 1) Down
      | Left -> aux (x - 1) y Left
      | Up when m.mirr = Horizontal ->
        aux (x - 1) y Left;
        aux (x + 1) y Right
      | Up when m.mirr = TopLeft -> aux (x - 1) y Left
      | Up when m.mirr = TopRight -> aux (x + 1) y Right
      | Up -> aux x (y - 1) Up
      | Down when m.mirr = Horizontal ->
        aux (x - 1) y Left;
        aux (x + 1) y Right
      | Down when m.mirr = TopRight -> aux (x - 1) y Left
      | Down when m.mirr = TopLeft -> aux (x + 1) y Right
      | Down -> aux x (y + 1) Down)
  in
  aux 0 0 Right
;;

let input = read_lines "inputs/16_t.txt" |> remove_empty_string
let mirrors = List.map String.explode input |> make_mirror_array
let () = print_newline ()
let () = print_newline ()
let () = print_newline ()
let () = print_mirrors mirrors
let () = print_newline ()
let () = print_newline ()
let () = energize_mirrors mirrors
let () = print_newline ()
let () = print_newline ()
let () = print_mirrors mirrors
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
  Printf.printf "Day 16 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
