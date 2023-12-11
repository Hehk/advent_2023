open Base
open Stdio

let test_file = Util.read_file (Util.data_dir 8 ^ "test.txt")
let test2_file = Util.read_file (Util.data_dir 8 ^ "test2.txt")
let test3_file = Util.read_file (Util.data_dir 8 ^ "test3.txt")
let input_file = Util.read_file (Util.data_dir 8 ^ "input.txt")
let empty = Map.empty (module String)

type raw_node = { name : string; left : string; right : string }

let parse_network_line line =
  let sections = String.split line ~on:'=' in
  let name = List.hd_exn sections |> String.strip in
  let leaves =
    List.nth_exn sections 1
    |> String.substr_replace_all ~pattern:"(" ~with_:""
    |> String.substr_replace_all ~pattern:")" ~with_:""
    |> String.split ~on:','
  in
  let left = List.hd_exn leaves |> String.strip in
  let right = List.nth_exn leaves 1 |> String.strip in
  { name; left; right }

let parse_input lines =
  let directions = List.hd_exn lines in
  let network =
    lines |> List.tl_exn |> List.tl_exn |> List.map ~f:parse_network_line
  in
  let network =
    List.fold network ~init:empty ~f:(fun acc node ->
        Map.set acc ~key:node.name ~data:node)
  in
  (directions, network)

let navigate_network ?node ?is_final_node directions network =
  let is_final_node =
    match is_final_node with Some f -> f | None -> String.equal "ZZZ"
  in

  let get_node = Map.find_exn network in
  let rec navigate_node steps = function
    | node when is_final_node node.name -> steps
    | node -> (
        let step_index = steps % String.length directions in
        let step = String.get directions step_index in
        match step with
        | 'R' -> navigate_node (steps + 1) (get_node node.right)
        | 'L' -> navigate_node (steps + 1) (get_node node.left)
        | _ -> failwith "Invalid step")
  in
  let first_node =
    match node with Some node -> node | None -> get_node "AAA"
  in
  navigate_node 0 first_node

let part1 input = input |> parse_input |> Util.uncurry navigate_network
let () = test_file |> part1 |> printf "Part 1, test 1: %d\n"
let () = test2_file |> part1 |> printf "Part 1, test 2: %d\n"
let () = input_file |> part1 |> printf "Part 1, input: %d\n"
let rec gcd a b = if b = 0 then a else gcd b (a % b)
let lcm a b = a * b / gcd a b

let navigate_network_2 directions network =
  let first_nodes =
    network |> Map.to_alist
    |> List.filter ~f:(fun (key, _) -> String.is_suffix ~suffix:"A" key)
    |> List.map ~f:snd
  in
  let is_final_node = String.is_suffix ~suffix:"Z" in
  let distances =
    List.map first_nodes ~f:(fun node ->
        navigate_network directions network ~node ~is_final_node)
  in
  List.fold distances ~init:1 ~f:lcm

let part2 input = input |> parse_input |> Util.uncurry navigate_network_2
let () = test3_file |> part2 |> printf "Part 2, test 3: %d\n"
let () = input_file |> part2 |> printf "Part 2, input: %d\n"
