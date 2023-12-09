open Base
open Stdio
open Domainslib

let data_dir = "data/day_5/"

type map_group = (int * int * int) list [@@deriving show]

type 'a data = {
  seeds : 'a;
  seed_to_soil : map_group;
  soil_to_fertilizer : map_group;
  fertilizer_to_water : map_group;
  water_to_light : map_group;
  light_to_temperature : map_group;
  temperature_to_humidity : map_group;
  humidity_to_location : map_group;
}
[@@deriving show]

let parse_map_section map line =
  if String.is_suffix line ~suffix:"map:" then map
  else if String.is_empty line then map
  else
    let nums = String.split line ~on:' ' |> List.map ~f:Int.of_string in
    let dest_start = List.nth_exn nums 0 in
    let source_start = List.nth_exn nums 1 in
    let range = List.nth_exn nums 2 in
    (dest_start, source_start, range) :: map

let part_1_parse_seeds =
  List.fold ~init:[] ~f:(fun acc line ->
      let line =
        String.substr_replace_first line ~pattern:"seeds: " ~with_:""
      in
      let seeds = String.split line ~on:' ' |> List.map ~f:Int.of_string in
      acc @ seeds)

let parse_groups parse_seeds lines =
  let () = print_endline "Parsing groups..." in
  let sections = List.group lines ~break:(fun _ line -> String.is_empty line) in
  let seeds_section = List.nth_exn sections 0 in
  let seeds = parse_seeds seeds_section in

  let () = print_endline "Parsing seed_to_soil..." in
  let seed_to_soil_section = List.nth_exn sections 1 in
  let seed_to_soil =
    List.fold seed_to_soil_section ~init:[] ~f:parse_map_section
  in

  let () = print_endline "Parsing soil_to_fertilizer..." in
  let soil_to_fertilizer_section = List.nth_exn sections 2 in
  let soil_to_fertilizer =
    List.fold soil_to_fertilizer_section ~init:[] ~f:parse_map_section
  in

  let () = print_endline "Parsing fertilizer_to_water..." in
  let fertilizer_to_water_section = List.nth_exn sections 3 in
  let fertilizer_to_water =
    List.fold fertilizer_to_water_section ~init:[] ~f:parse_map_section
  in

  let () = print_endline "Parsing water_to_light..." in
  let water_to_light_section = List.nth_exn sections 4 in
  let water_to_light =
    List.fold water_to_light_section ~init:[] ~f:parse_map_section
  in

  let () = print_endline "Parsing light_to_temperature..." in
  let light_to_temperature_section = List.nth_exn sections 5 in
  let light_to_temperature =
    List.fold light_to_temperature_section ~init:[] ~f:parse_map_section
  in

  let () = print_endline "Parsing temperature_to_humidity..." in
  let temperature_to_humidity_section = List.nth_exn sections 6 in
  let temperature_to_humidity =
    List.fold temperature_to_humidity_section ~init:[] ~f:parse_map_section
  in

  let () = print_endline "Parsing humidity_to_location..." in
  let humidity_to_location_section = List.nth_exn sections 7 in
  let humidity_to_location =
    List.fold humidity_to_location_section ~init:[] ~f:parse_map_section
  in

  {
    seeds;
    seed_to_soil;
    soil_to_fertilizer;
    fertilizer_to_water;
    water_to_light;
    light_to_temperature;
    temperature_to_humidity;
    humidity_to_location;
  }

let get map key =
  let new_val =
    List.fold map ~init:key ~f:(fun acc (dest_start, source_start, range) ->
        if key >= source_start && key < source_start + range then
          let diff = key - source_start in
          let new_val = dest_start + diff in
          new_val
        else acc)
  in
  (* let () = printf "Key: %d, New val: %d\n" key new_val in *)
  new_val

let calculate_locations data =
  let () = print_endline "Calculating locations..." in
  List.map data.seeds ~f:(fun seed ->
      seed |> get data.seed_to_soil
      |> get data.soil_to_fertilizer
      |> get data.fertilizer_to_water
      |> get data.water_to_light
      |> get data.light_to_temperature
      |> get data.temperature_to_humidity
      |> get data.humidity_to_location)

let min_location = List.min_elt ~compare:Int.compare

let () =
  Util.read_file (data_dir ^ "test.txt")
  |> parse_groups part_1_parse_seeds
  |> calculate_locations |> min_location |> Option.value_exn
  |> printf "Part 1, Lowest seed location: %d\n"

let part_2_parse_seeds lines =
  lines
  |> List.fold ~init:[] ~f:(fun acc line ->
         let line =
           String.substr_replace_first line ~pattern:"seeds: " ~with_:""
         in
         let seeds = String.split line ~on:' ' |> List.map ~f:Int.of_string in
         acc @ seeds)
  |> List.chunks_of ~length:2
  |> List.map ~f:(function [ start; range ] -> [ (start, range) ] | _ -> [])
  |> List.concat

let calc_location data seed =
  seed |> get data.seed_to_soil
  |> get data.soil_to_fertilizer
  |> get data.fertilizer_to_water
  |> get data.water_to_light
  |> get data.light_to_temperature
  |> get data.temperature_to_humidity
  |> get data.humidity_to_location

let create_chunks ~chunk_size start range =
  let num_chunks = (range / chunk_size) + 1 in
  List.range 0 num_chunks
  |> List.map ~f:(fun chunk ->
         let new_start = (chunk * chunk_size) + start in
         if new_start + chunk_size > start + range then
           (new_start, start + range - new_start)
         else (new_start, chunk_size))

let efficient_calculate_locations pool data =
  let () = print_endline "Calculating locations..." in
  let calc_location = calc_location data in
  let min_loc =
    List.map data.seeds ~f:(fun (start, range) ->
        let chunks = create_chunks ~chunk_size:100_000 start range in
        let min_loc =
          List.map chunks ~f:(fun (start, range) ->
              Task.async pool (fun _ ->
                  let min_loc = ref Int.max_value in
                  for seed = start to start + range do
                    let loc = calc_location seed in
                    if loc < !min_loc then min_loc := loc
                  done;
                  !min_loc))
          |> List.map ~f:(fun t ->
                 try Task.await pool t
                 with e ->
                   printf "Error: %s\n" (Exn.to_string e);
                   Int.max_value)
          |> min_location
        in
        match min_loc with Some min_loc -> min_loc | None -> Int.max_value)
    |> min_location
  in
  Task.teardown_pool pool;
  min_loc

let pool = Task.setup_pool ~num_domains:4 ()

let () =
  let data =
    Util.read_file (data_dir ^ "input.txt") |> parse_groups part_2_parse_seeds
  in
  Task.run pool (fun _ -> efficient_calculate_locations pool data)
  |> Option.value_exn
  |> printf "Part 2, Lowest seed location: %d\n"
