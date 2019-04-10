open Core
open Core_bench
open Stdio


(* -- HELPERS --------------------------------------------------------------- *)


let box_value box =
  let has_count n counts = Bool.to_int (List.exists ~f:((=) n) counts) in
  let counts = Hashtbl.create (module Char) ~size:(String.length box) in
  String.iter box ~f:(fun k ->
      Hashtbl.update counts k ~f:(fun v -> 1 + Option.value ~default:0 v)
    );
  let counts = Hashtbl.data counts in
  ( has_count 2 counts, has_count 3 counts )

let string_zip_exn s1 s2 =
  let n = String.length s1 in
  let rec loop i ac =
    if i < 0 then ac else loop (i - 1) ((s1.[i], s2.[i]) :: ac)
  in
  loop (n - 1) []

let one_char_diff_exn s1 s2 =
  let n = String.length s1 in
  let rec loop i diff_chars =
    if i = n then diff_chars = 1
    else if Char.(s1.[i] <> s2.[i]) then
      if diff_chars = 1 then false else loop (i + 1) (diff_chars + 1)
    else
      loop (i + 1) diff_chars
  in
  loop 0 0

let keep_shared_chars s1 s2 =
  let char_lists = string_zip_exn s1 s2 in
  List.fold_right char_lists ~init:[] ~f:(fun (x,y) acc ->
      if Char.(x = y) then x :: acc else acc
    )
  |> String.of_char_list


(* -- AOC 2018 DAY 2 -------------------------------------------------------- *)


let part_one boxes =
  let sum (x,y) (x',y') = (x + x', y + y') in
  List.fold boxes ~init:(0,0) ~f:(fun ac box -> sum ac (box_value box))
  |> (fun (twos, threes) -> twos * threes)

let part_two boxes ~default =
  let rec loop = function
    | [] -> default
    | box :: boxes ->
      match List.find boxes ~f:(one_char_diff_exn box) with
      | None -> loop boxes
      | Some box' -> keep_shared_chars box box'
  in
  loop boxes

let () =
  In_channel.with_file "input.txt" ~f:(fun inc ->
      let lines = In_channel.input_lines inc in
      let exec_part_one () = part_one lines in
      let exec_part_two () = part_two lines ~default:"" in
      print_endline "RESULTS:";
      printf "Part 1: %d\n" (exec_part_one ());
      printf "Part 2: %s\n" (exec_part_two ());
      print_endline "\nBENCHMARKS:";
      [ Bench.Test.create ~name:"Part 1" exec_part_one;
        Bench.Test.create ~name:"Part 2" exec_part_two;
      ]
      |> Bench.make_command
      |> Command.run
    )
