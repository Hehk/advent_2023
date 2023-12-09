open Base
open Stdio

let data_dir = "data/day_2"

let read_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      In_channel.fold_lines ic ~init:[] ~f:(fun acc line -> line :: acc)
      |> List.rev)

type game = { id : int; sets : (int * int * int) list }

let parse_set set =
  String.split set ~on:',' |> List.map ~f:String.strip
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(function
       | [ n; "red" ] -> (Int.of_string n, 0, 0)
       | [ n; "green" ] -> (0, Int.of_string n, 0)
       | [ n; "blue" ] -> (0, 0, Int.of_string n)
       | _ -> failwith "invalid set")
  |> List.fold ~init:(0, 0, 0) ~f:(fun (r, g, b) (r', g', b') ->
         (r + r', g + g', b + b'))

let parse_id id = String.drop_prefix id 4 |> String.strip |> Int.of_string

let parse_line line =
  let sections = String.split ~on:':' line in
  let id = List.hd_exn sections |> parse_id in
  let sets = List.nth_exn sections 1 |> String.strip in
  let sets = String.split sets ~on:';' |> List.map ~f:parse_set in
  { id; sets }

let is_valid game (limit_r, limit_g, limit_b) =
  List.for_all game.sets ~f:(fun (r, g, b) ->
      r <= limit_r && g <= limit_g && b <= limit_b)

let () =
  read_file (data_dir ^ "/input_1.txt")
  |> List.map ~f:parse_line
  |> List.filter ~f:(fun game -> is_valid game (12, 13, 14))
  |> List.fold ~init:0 ~f:(fun acc game -> acc + game.id)
  |> printf "Part 1, sum of ids: %d\n"

let min_set game =
  List.fold game.sets ~init:(0, 0, 0) ~f:(fun (r, g, b) (r', g', b') ->
      (max r r', max g g', max b b'))

let power_of_set (r, g, b) = r * g * b

let () =
  read_file (data_dir ^ "/input_1.txt")
  |> List.map ~f:parse_line |> List.map ~f:min_set |> List.map ~f:power_of_set
  |> List.fold ~init:0 ~f:( + )
  |> printf "Part 2, sum of powers: %d\n"
