open Core
open Core_bench
open Stdio

let part_one = List.fold ~f:( + ) ~init:0

let part_two ints =
  let size = Int.max 10_000 (List.length ints) in
  let prev_sums = Hash_set.create (module Int) ~size () in
  Sequence.cycle_list_exn ints
  |> Sequence.fold_until ~init:0
       ~finish:(fun _ -> assert false)
       ~f:(fun prev_sum int ->
         let new_sum = prev_sum + int in
         if Hash_set.mem prev_sums new_sum then Stop new_sum
         else (
           Hash_set.add prev_sums new_sum ;
           Continue new_sum ) )

let () =
  In_channel.with_file "input.txt" ~f:(fun inc ->
      let ints = List.map ~f:Int.of_string (In_channel.input_lines inc) in
      let exec_part_one () = part_one ints in
      let exec_part_two () = part_two ints in
      print_endline "RESULTS:" ;
      printf "Part 1: %d\n" (exec_part_one ()) ;
      printf "Part 2: %d\n" (exec_part_two ()) ;
      print_endline "\nBENCHMARKS:" ;
      [ Bench.Test.create ~name:"Part 1" exec_part_one
      ; Bench.Test.create ~name:"Part 2" exec_part_two ]
      |> Bench.make_command |> Command.run )
