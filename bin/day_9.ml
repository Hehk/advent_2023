open Base
open Stdio
open Util

let data_dir = data_dir 9
let test_input = read_file (data_dir ^ "test.txt")
let test2_input = read_file (data_dir ^ "test2.txt")
let input = read_file (data_dir ^ "input.txt")

let rec compute_diff_lists nums =
  let nums_diff =
    List.range 0 (List.length nums - 1)
    |> List.map ~f:(fun i -> List.nth_exn nums (i + 1) - List.nth_exn nums i)
  in
  if List.for_all nums_diff ~f:(fun x -> x = 0) then [ nums_diff ]
  else nums_diff :: compute_diff_lists nums_diff

let compute_next_number nums =
  let diff_lists = compute_diff_lists nums in
  let next_num =
    diff_lists |> List.map ~f:List.last_exn
    |> List.fold ~init:(List.last_exn nums) ~f:( + )
  in
  next_num

let part_1 lines =
  lines
  |> List.map ~f:(compute_next_number % parse_num_list)
  |> List.fold ~f:( + ) ~init:0

let () = part_1 test_input |> printf "Part 1 Test: %d\n"
let () = part_1 input |> printf "Part 1: %d\n"

let compute_prev_number nums =
  let diff_lists = compute_diff_lists nums in
  let diff =
    diff_lists |> List.rev
    |> List.fold ~init:0 ~f:(fun acc diffs ->
           let hd = List.hd_exn diffs in
           hd - acc)
  in
  let next_num = List.hd_exn nums - diff in
  let () = printf "next_num: %d\n" next_num in
  next_num

let part_2 lines =
  lines
  |> List.map ~f:(compute_prev_number % parse_num_list)
  |> print_int_list |> List.fold ~f:( + ) ~init:0

let () =
  test2_input |> print_string_list |> part_2 |> printf "Part 2 Test: %d\n"

let () = input |> print_string_list |> part_2 |> printf "Part 2 Test: %d\n"
