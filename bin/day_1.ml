open Base
open Stdio

let data_dir = "/Users/kyle/Projects/advent_2023/data/day_1/"

let read_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      In_channel.fold_lines ic ~init:[] ~f:(fun acc line -> line :: acc)
      |> List.rev)

let decode_line line =
  let nums =
    String.filter line ~f:(function
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
      | _ -> false)
  in
  let fst = String.get nums 0 |> Char.to_string |> Int.of_string in
  let last =
    String.get nums (String.length nums - 1) |> Char.to_string |> Int.of_string
  in
  (fst * 10) + last

let replace_spelled_numbers line =
  String.substr_replace_all line ~pattern:"zero" ~with_:"zero0zero"
  |> String.substr_replace_all ~pattern:"one" ~with_:"one1one"
  |> String.substr_replace_all ~pattern:"two" ~with_:"two2two"
  |> String.substr_replace_all ~pattern:"three" ~with_:"three3three"
  |> String.substr_replace_all ~pattern:"four" ~with_:"four4four"
  |> String.substr_replace_all ~pattern:"five" ~with_:"five5five"
  |> String.substr_replace_all ~pattern:"six" ~with_:"six6six"
  |> String.substr_replace_all ~pattern:"seven" ~with_:"seven7seven"
  |> String.substr_replace_all ~pattern:"eight" ~with_:"eight8eight"
  |> String.substr_replace_all ~pattern:"nine" ~with_:"nine9nine"

let decode_line_2 line =
  let line = replace_spelled_numbers line in
  decode_line line

let () =
  read_file (data_dir ^ "part_1.txt")
  |> List.map ~f:decode_line |> List.fold ~init:0 ~f:( + )
  |> printf "Part 1, Sum of all of the calibration values: %i\n"

let () =
  read_file (data_dir ^ "part_1.txt")
  |> List.map ~f:decode_line_2 (* |> List.iter ~f:(printf "%i\n") *)
  |> List.fold ~init:0 ~f:( + )
  |> printf "Part 2, Sum of all of the calibration values: %i\n"
