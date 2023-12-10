open Base
open Stdio

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
