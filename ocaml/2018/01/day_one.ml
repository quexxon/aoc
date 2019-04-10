open Core
open Core_bench
open Stdio

let part_one = List.fold ~f:(+) ~init:0

let part_two ints =
  let size = Int.max 10_000 (List.length ints) in
  let prev_sums = Hash_set.create (module Int) ~size () in
  Sequence.cycle_list_exn ints
  |> Sequence.fold_until ~init:0
    ~finish:(fun _ -> assert false)
    ~f:(fun prev_sum int ->
        let new_sum = prev_sum + int in
        if Hash_set.mem prev_sums new_sum then
          Stop new_sum
        else begin
          Hash_set.add prev_sums new_sum;
          Continue new_sum
        end
      )

let () =
  In_channel.with_file "input.txt" ~f:(fun inc ->
      let ints = List.map ~f:Int.of_string (In_channel.input_lines inc) in
      print_endline "RESULTS:";
      printf "Part 1: %d\n" (part_one ints);
      printf "Part 2: %d\n" (part_two ints);
      print_endline "\nBENCHMARKS:";
      [ Bench.Test.create ~name:"Part 1"
          (fun () -> part_one ints);
        Bench.Test.create ~name:"Part 2"
          (fun () -> part_two ints);
      ]
      |> Bench.make_command
      |> Command.run
    )
