open Base
open Stdio

let read_file filename =
  In_channel.with_file filename ~f:(fun ic ->
    In_channel.fold_lines ic ~init:[] ~f:(fun acc line -> line :: acc)
    |> List.rev
  )
