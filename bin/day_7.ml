open Base
open Stdio

let data_dir = Util.data_dir 7

module Card = struct
  type t =
    | Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    | Joker
  [@@deriving show]

  let of_char = function
    | 'A' -> Ace
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Jack
    | 'T' -> Ten
    | '9' -> Nine
    | '8' -> Eight
    | '7' -> Seven
    | '6' -> Six
    | '5' -> Five
    | '4' -> Four
    | '3' -> Three
    | '2' -> Two
    (* Gave it an arbitrar character for the joker *)
    | 'R' -> Joker
    | _ -> failwith "Invalid card"

  let card_value = function
    | Ace -> 14
    | King -> 13
    | Queen -> 12
    | Jack -> 11
    | Ten -> 10
    | Nine -> 9
    | Eight -> 8
    | Seven -> 7
    | Six -> 6
    | Five -> 5
    | Four -> 4
    | Three -> 3
    | Two -> 2
    | Joker -> 1

  let compare c1 c2 =
    let v1 = card_value c1 in
    let v2 = card_value c2 in
    if v1 > v2 then 1 else if v1 < v2 then -1 else 0
end

module Hand = struct
  type kind =
    | FiveOfAKind
    | FourOfAKind
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | FullHouse
    | HighCard
  [@@deriving show]

  let kind_value = function
    | FiveOfAKind -> 9
    | FourOfAKind -> 8
    | FullHouse -> 7
    | ThreeOfAKind -> 6
    | TwoPair -> 5
    | OnePair -> 4
    | HighCard -> 3

  let kind_compare a b = Int.compare (kind_value a) (kind_value b)

  type t = { cards : Card.t list; bid : int; kind : kind } [@@deriving show]

  let compare a b =
    let kind_compare = kind_compare a.kind b.kind in
    if kind_compare <> 0 then kind_compare
    else
      let first_diff =
        List.find_map (List.zip_exn a.cards b.cards) ~f:(fun (c1, c2) ->
            let c = Card.compare c1 c2 in
            if c <> 0 then Some c else None)
      in
      match first_diff with Some c -> c | None -> 0
end

let group_by_value cards =
  List.group cards ~break:(fun c1 c2 -> Card.compare c1 c2 <> 0)

let calc_hand_kind cards =
  let open Hand in
  let sorted_cards = List.sort cards ~compare:Card.compare in
  let groups = group_by_value sorted_cards in
  let group_lengths =
    List.map groups ~f:List.length |> List.sort ~compare:Int.compare |> List.rev
  in
  match group_lengths with
  | [ 1; 1; 1; 1; 1 ] -> HighCard
  | [ 2; 1; 1; 1 ] -> OnePair
  | [ 2; 2; 1 ] -> TwoPair
  | [ 3; 1; 1 ] -> ThreeOfAKind
  | [ 3; 2 ] -> FullHouse
  | [ 4; 1 ] -> FourOfAKind
  | [ 5 ] -> FiveOfAKind
  | _ -> failwith "Invalid hand"

let calc_hand_kind_with_joker cards =
  let possible_cards =
    [
      Card.Ace;
      Card.King;
      Card.Queen;
      Card.Ten;
      Card.Nine;
      Card.Eight;
      Card.Seven;
      Card.Six;
      Card.Five;
      Card.Four;
      Card.Three;
      Card.Two;
    ]
  in
  let kinds =
    List.map possible_cards ~f:(fun c ->
        let new_cards =
          List.map cards ~f:(fun c' ->
              if phys_equal c' Card.Joker then c else c')
        in
        calc_hand_kind new_cards)
  in
  let sorted_kinds = List.sort kinds ~compare:Hand.kind_compare in
  List.last_exn sorted_kinds

let parse_hand line =
  let sections = String.split line ~on:' ' in
  let cards =
    List.hd_exn sections |> String.to_list |> List.map ~f:Card.of_char
  in
  let bid = List.nth_exn sections 1 |> Int.of_string in
  if List.exists cards ~f:(fun c -> phys_equal c Card.Joker) then
    let kind = calc_hand_kind_with_joker cards in
    Hand.{ cards; bid; kind }
  else
    let kind = calc_hand_kind cards in
    Hand.{ cards; bid; kind }

let parse_joker_hand line =
  let sections = String.split line ~on:' ' in
  let cards =
    List.hd_exn sections |> String.to_list |> List.map ~f:Card.of_char
  in
  let bid = List.nth_exn sections 1 |> Int.of_string in
  let kind = calc_hand_kind cards in
  Hand.{ cards; bid; kind }

let () =
  Util.read_file (data_dir ^ "input.txt")
  |> List.map ~f:parse_hand
  |> List.sort ~compare:Hand.compare
  |> List.foldi ~init:0 ~f:(fun i acc hand ->
         let bid = hand.Hand.bid in
         let hand_value = (i + 1) * bid in
         acc + hand_value)
  |> printf "Part 1, Total Winnings: %d\n"

let patch_jokers = String.substr_replace_all ~pattern:"J" ~with_:"R"

let () =
  Util.read_file (data_dir ^ "input.txt")
  |> List.map ~f:patch_jokers |> List.map ~f:parse_hand
  |> List.sort ~compare:Hand.compare
  |> List.foldi ~init:0 ~f:(fun i acc hand ->
         let bid = hand.Hand.bid in
         let hand_value = (i + 1) * bid in
         acc + hand_value)
  |> printf "Part 2, Total Winnings: %d\n"
