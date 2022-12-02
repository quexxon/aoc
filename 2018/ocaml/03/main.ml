open Core
open Core_bench
open Stdio

let part_one fabric = Fabric.count_occupants fabric ~f:(( < ) 1)

let part_two fabric =
  let single_occupant point = Some 1 = Fabric.occupants_at fabric point in
  match Fabric.find_claim fabric ~f:(Claim.for_all ~f:single_occupant) with
  | None -> raise (Not_found_s (String.sexp_of_t "No Match"))
  | Some claim -> Claim.id claim

let () =
  In_channel.with_file "input.txt" ~f:(fun inc ->
      let lines = In_channel.input_lines inc in
      let exec_setup () =
        List.map lines ~f:(fun line -> Option.value_exn (Claim.parse line))
        |> Fabric.make
      in
      let fabric = exec_setup () in
      let exec_part_one () = part_one fabric in
      let exec_part_two () = part_two fabric in
      print_endline "RESULTS:" ;
      printf "Part 1: %d\n" (exec_part_one ()) ;
      printf "Part 2: %d\n" (exec_part_two ()) ;
      print_endline "\nBENCHMARKS" ;
      [ Bench.Test.create ~name:"Setup" exec_setup
      ; Bench.Test.create ~name:"Part 1" exec_part_one
      ; Bench.Test.create ~name:"Part 2" exec_part_two ]
      |> Bench.make_command |> Command.run )
