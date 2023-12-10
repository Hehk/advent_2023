open Base
open Stdio

let data_dir = "data/day_6/"
let ( % ) f g x = f (g x)

let extract_nums line =
  line |> String.strip |> String.split ~on:' '
  |> List.filter ~f:(not % String.is_empty)
  |> List.map ~f:Int.of_string

let remove_title title line =
  line |> String.substr_replace_first ~pattern:(title ^ ":") ~with_:""

let parse_input lines =
  let time_line = List.hd_exn lines in
  let distance_line = List.nth_exn lines 1 in
  let times = time_line |> remove_title "Time" |> extract_nums in
  let distances = distance_line |> remove_title "Distance" |> extract_nums in
  List.zip_exn times distances

let calc_valid_plays (time, distance) =
  let times = List.range 0 time in
  let is_valid_time t =
    let remaining_time = time - t in
    let total_distance = remaining_time * t in
    total_distance > distance
  in
  let valid_times = List.filter times ~f:is_valid_time in
  (* let () = *)
  (*   List.iter valid_times ~f:(fun t -> *)
  (*       printf "%d %d Time: %d\n" time distance t) *)
  (* in *)
  valid_times

let () =
  Util.read_file (data_dir ^ "input.txt")
  |> parse_input
  |> List.map ~f:calc_valid_plays
  |> List.map ~f:List.length |> List.fold ~init:1 ~f:( * )
  |> printf "Part1, result: %d\n"

let extract_single_num line =
  line |> String.substr_replace_all ~pattern:" " ~with_:"" |> Int.of_string

let parse_input_2 lines =
  let time_line = List.hd_exn lines in
  let distance_line = List.nth_exn lines 1 in
  let times = time_line |> remove_title "Time" |> extract_single_num in
  let distances =
    distance_line |> remove_title "Distance" |> extract_single_num
  in
  [ (times, distances) ]

let () =
  Util.read_file (data_dir ^ "input.txt")
  |> parse_input_2
  |> List.map ~f:calc_valid_plays
  |> List.map ~f:List.length |> List.fold ~init:1 ~f:( * )
  |> printf "Part2, result: %d\n"
