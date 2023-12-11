open Base
open Stdio

let ( % ) f g x = f (g x)
let ( %> ) f g x = g (f x)
let uncurry f (x, y) = f x y
let ignore _ = ()

let read_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      In_channel.fold_lines ic ~init:[] ~f:(fun acc line -> line :: acc)
      |> List.rev)

let print_name = function None -> () | Some name -> printf "%s: " name

let print_string_list ?name lst =
  print_name name;
  List.iter lst ~f:(fun s -> printf "%s, " s);
  print_endline "";
  lst

let print_int_list ?name lst =
  print_name name;
  List.iter lst ~f:(fun s -> printf "%d, " s);
  print_endline "";
  lst

let data_dir n = "data/day_" ^ Int.to_string n ^ "/"

let parse_num_list ?sep str =
  let sep = Option.value ~default:' ' sep in
  String.split str ~on:sep
  |> List.filter ~f:(not % String.is_empty)
  |> List.map ~f:Int.of_string
