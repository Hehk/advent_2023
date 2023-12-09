open Base
open Stdio
open Re

let data_dir = "data/day_3"

let parse_file lines =
  let text = List.fold lines ~init:"" ~f:(fun acc line -> acc ^ line) in
  let row_length = String.length (List.hd_exn lines) in
  (text, row_length)

type part_number = { num : int; start : int; stop : int; valid : bool }
[@@deriving show]

let format_matches group =
  let num = Re.Group.get group 0 in
  let num = Int.of_string num in
  let start = Re.Group.start group 0 in
  let stop = Re.Group.stop group 0 in
  { num; start; stop; valid = false }

let is_index_within start stop i = start <= i && i < stop

let prevent_wrapping row_length valid_index i =
  let row = valid_index / row_length in
  let test_row = i / row_length in
  if row <> test_row then valid_index else i

let is_touching start stop row_length i =
  let wrap = prevent_wrapping row_length in
  (* Top row *)
  let a, b, c = (i - row_length - 1, i - row_length, i - row_length + 1) in
  let a, c = (wrap b a, wrap b c) in
  (* Middle row *)
  let d, e, f = (i - 1, i, i + 1) in
  let d, f = (wrap e d, wrap e f) in
  (* Bottom row *)
  let g, h, j = (i + row_length - 1, i + row_length, i + row_length + 1) in
  let g, j = (wrap h g, wrap h j) in

  List.exists [ a; b; c; d; e; f; g; h; j ] ~f:(fun i ->
      is_index_within start stop i)

let is_symbol = function '.' | '0' .. '9' -> false | _ -> true

let check_symbol row_length i part_numbers c =
  if not (is_symbol c) then part_numbers
  else
    let part_numbers =
      List.map part_numbers ~f:(fun part_number ->
          if is_touching part_number.start part_number.stop row_length i then
            { part_number with valid = true }
          else part_number)
    in
    part_numbers

let get_part_numbers text =
  let regex = Re.Perl.compile_pat "\\d+" in
  let part_numbers = Re.all regex text in
  let part_numbers = List.map part_numbers ~f:format_matches in
  part_numbers

let valid_part_numbers (text, row_length) =
  let part_numbers = get_part_numbers text in
  let part_numbers =
    String.foldi ~init:part_numbers text ~f:(check_symbol row_length)
  in
  part_numbers

let sum_part_numbers part_numbers =
  (* sizeof part_numbers *)
  let () = printf "Number of part numbers: %d\n" (List.length part_numbers) in
  (* num of valid part numbers *)
  let () =
    printf "Number of valid part numbers: %d\n"
      (List.length (List.filter part_numbers ~f:(fun x -> x.valid)))
  in
  List.fold part_numbers ~init:0 ~f:(fun acc part_number ->
      if part_number.valid then acc + part_number.num else acc)

let () =
  Util.read_file (data_dir ^ "/input.txt")
  |> parse_file |> valid_part_numbers |> sum_part_numbers
  |> printf "Sum of all of the part numbers in the engine schematic: %d\n"

let get_gear_ratio part_numbers row_length i =
  let part_numbers =
    List.map part_numbers ~f:(fun part_number ->
        if is_touching part_number.start part_number.stop row_length i then
          { part_number with valid = true }
        else part_number)
  in
  let valid_part_numbers = List.filter part_numbers ~f:(fun x -> x.valid) in
  if List.length valid_part_numbers < 2 then 0
  else
    List.fold valid_part_numbers ~init:1 ~f:(fun acc part_number ->
        acc * part_number.num)

let check_gear part_numbers row_length i gears = function
  | '*' -> get_gear_ratio part_numbers row_length i :: gears
  | _ -> gears

let gear_ratios (text, row_length) =
  let part_numbers = get_part_numbers text in
  let ratios =
    String.foldi ~init:[] text ~f:(check_gear part_numbers row_length)
  in
  let () =
    printf "Number of valid gears: %d\n"
      (List.length (List.filter ratios ~f:(fun x -> x <> 0)))
  in
  ratios

let () =
  Util.read_file (data_dir ^ "/input.txt")
  |> parse_file |> gear_ratios |> List.fold ~init:0 ~f:( + )
  |> printf "Sum of all of the gear ratios in the engine schematic: %d\n"
