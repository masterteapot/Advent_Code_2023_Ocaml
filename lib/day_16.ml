open Batteries
open Utilities
(* open Printf *)

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
  ; mutable up : bool
  ; mutable down : bool
  ; mutable left : bool
  ; mutable right : bool
  }

let print_mirrors mirrors =
  Array.iter
    (fun x ->
      print_newline ();
      Array.iter (fun y -> Printf.printf " %c " (if y.energized then '#' else y.symbol)) x)
    mirrors
;;

let make_mirror_array input =
  let m =
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
          ; up = false
          ; down = false
          ; left = false
          ; right = false
          })
        x)
    m
;;

let energize_mirrors m start =
  Array.iter
    (Array.iter (fun x ->
       x.energized <- false;
       x.left <- false;
       x.right <- false;
       x.down <- false;
       x.up <- false))
    m;
  let count_energies mir =
    Array.fold_left
      (fun acc x ->
        Array.fold_left (fun bcc y -> if y.energized then 1 + bcc else bcc) 0 x + acc)
      0
      mir
  in
  let max_y = Array.length m in
  let max_x = Array.length m.(0) in
  let rec aux x y dir =
    if x < 0
       || x >= max_x
       || y < 0
       || y >= max_y
       || (dir = Right && m.(y).(x).right)
       || (dir = Down && m.(y).(x).down)
       || (dir = Left && m.(y).(x).left)
       || (dir = Up && m.(y).(x).up)
    then ()
    else (
      m.(y).(x).energized <- true;
      if dir = Right
      then m.(y).(x).right <- true
      else if dir = Left
      then m.(y).(x).left <- true
      else if dir = Up
      then m.(y).(x).up <- true
      else if dir = Down
      then m.(y).(x).down <- true;
      let m = m.(y).(x) in
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
  aux (Tuple3.first start) (Tuple3.second start) (Tuple3.third start);
  count_energies m
;;

let calc_start_directions m =
  let y_max = pred @@ Array.length m in
  let x_max = pred @@ Array.length m.(0) in
  let vert = List.of_enum @@ Int.( --- ) 0 y_max in
  let hori = List.of_enum @@ Int.( --- ) 0 x_max in
  let rec aux ls base_val axis dir acc =
    match ls with
    | [] -> List.rev acc
    | hd :: tl ->
      let out = if axis = "x" then hd, base_val, dir else base_val, hd, dir in
      aux tl base_val axis dir (out :: acc)
  in
  let left = aux vert 0 "y" Right [] in
  let right = aux vert y_max "y" Left [] in
  let top = aux hori 0 "x" Down [] in
  let bottom = aux hori x_max "x" Up [] in
  left @ right @ top @ bottom
;;

let () = print_newline ()

(* Part 1 *)
let part_one () =
  let input = read_lines "inputs/16_t.txt" |> remove_empty_string in
  let mirrors = List.map String.explode input |> make_mirror_array in
  let output = energize_mirrors mirrors (0, 0, Right) in
  let out_1 = output in
  Printf.printf "Day 16 Part 1 --> %d\n" out_1
;;

(* Part 2 *)
let part_two () =
  let input = read_lines "inputs/16.txt" |> remove_empty_string in
  let mirrors = List.map String.explode input |> make_mirror_array in
  let start_directions = calc_start_directions mirrors in
  let output =
    List.fold_left
      (fun acc x ->
        if acc > energize_mirrors mirrors x then acc else energize_mirrors mirrors x)
      0
      start_directions
  in
  let out_2 = output in
  Printf.printf "Day 16 Part 2 --> %d\n" out_2
;;

let main () =
  part_one ();
  part_two ()
;;
