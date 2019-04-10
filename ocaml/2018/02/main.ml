open Core
open Core_bench
open Stdio

let box_value id =
  let init = Map.empty (module Char) in
  let counts =
    String.fold id ~init ~f:(fun counts key ->
        Map.update counts key ~f:(fun v -> 1 + Option.value ~default:0 v)
      )
    |> Map.data
  in
  let has_count n = Bool.to_int (List.exists ~f:((=) n) counts) in
  ( has_count 2, has_count 3 )

let part_one input =
  let (twos,threes) =
    List.fold input ~init:(0,0) ~f:(fun (twos,threes) box_id ->
        let (twos',threes') = box_value box_id in
        (twos + twos', threes + threes')
      )
  in twos * threes

let eql_but_one s1 s2 =
  let char_lists = List.zip_exn s1 s2 in
  List.fold_until char_lists ~init:0 ~finish:((=) 1) ~f:(fun diff (x,y) ->
      if Char.(x <> y) then
        if diff = 1 then
          Stop false
        else
          Continue (diff + 1)
      else
        Continue diff
    )

let keep_shared_chars s1 s2 =
  let char_lists = List.zip_exn s1 s2 in
  List.fold_right char_lists ~init:[] ~f:(fun (x,y) acc ->
      if Char.(x = y) then x :: acc else acc
    )
  |> String.of_char_list

let part_two input =
  let input = List.map ~f:String.to_list input in
  List.fold_until input ~init:(List.tl input) ~finish:(Fn.const "")
    ~f:(fun tl_opt hd ->
      match tl_opt with
      | None -> Continue None
      | Some tl ->
         match List.find tl ~f:(eql_but_one hd) with
         | None -> Continue (List.tl tl)
         | Some x -> Stop (keep_shared_chars hd x)
    )


let () =
  In_channel.with_file "input.txt" ~f:(fun inc ->
      let lines = In_channel.input_lines inc in
      print_endline "RESULTS:";
      printf "Part 1: %d\n" (part_one lines);
      printf "Part 2: %s\n" (part_two lines);
      print_endline "\nBENCHMARKS:";
      [ Bench.Test.create ~name:"Part 1"
          (fun () -> part_one lines);
        Bench.Test.create ~name:"Part 2"
          (fun () -> part_two lines);
      ]
      |> Bench.make_command
      |> Command.run
    )
