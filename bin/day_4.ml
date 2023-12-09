open Base
open Stdio

let data_dir = "data/day_4/"

let parse_input_nums raw =
  let nums = String.split raw ~on:' ' in
  let nums = List.filter nums ~f:(fun num -> not (String.is_empty num)) in
  List.map nums ~f:Int.of_string

let parse_line line =
  let sections = String.split line ~on:':' in
  let card_info = List.nth_exn sections 1 |> String.split ~on:'|' in
  let winning_nums = card_info |> List.hd_exn |> parse_input_nums in
  let your_nums = List.nth_exn card_info 1 |> parse_input_nums in
  (winning_nums, your_nums)

let print_nums nums =
  let nums = List.map nums ~f:Int.to_string in
  let nums = List.fold nums ~init:"" ~f:(fun acc num -> acc ^ " " ^ num) in
  printf "%s\n" nums

let compute_matches winning_nums your_nums =
  let winning_nums = Set.of_list (module Int) winning_nums in
  let matches = List.filter your_nums ~f:(Set.mem winning_nums) in
  List.length matches

let compute_score = function
  | x when x > 0 -> 2 ** (x - 1)
  | _ -> 0

let calc_matches =
  List.map ~f:(fun (winning_nums, your_nums) ->
      compute_matches winning_nums your_nums)

let calc_scores = List.map ~f:compute_score

let parse_input = List.map ~f:parse_line

let () =
  Util.read_file (data_dir ^ "input.txt")
  |> parse_input
  |> calc_matches
  |> calc_scores
  |> List.fold ~init:0 ~f:( + )
  |> printf "Part 1, Total Points: %d\n"

let calc_card_count matches =
  let card_count = List.fold matches ~init:[] ~f:(fun acc m ->
    (m, 1) :: acc
  ) |> List.rev in
  let card_count = List.foldi card_count ~init:card_count ~f:(fun i acc (m, _) ->
    let c = List.nth_exn acc i |> snd in
    match m with
    | m when m > 0 ->
        List.mapi acc ~f:(fun i' (m', c') ->
          match i' with
          | i' when i' <= i -> (m', c')
          | i' when i' <= i + m -> (m', c' + c)
          | _ -> (m', c')
        )
    | _ -> acc
  ) in
  List.map card_count ~f:(snd)



let () =
  Util.read_file (data_dir ^ "input.txt")
  |> parse_input
  |> calc_matches
  |> calc_card_count
  |> List.fold ~init:0 ~f:( + )
  |> printf "Part 2, Total Points: %d\n"
