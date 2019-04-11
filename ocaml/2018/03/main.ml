open Core
open Stdio

module IntPair = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Hashable (Int) (Int)
end

module Claim = struct
  exception Invalid_claim of string

  type t =
    { id : int
    ; pos : IntPair.t
    ; size : IntPair.t }

  let parse s =
    let open Option.Monad_infix in
    let re = Re.Perl.compile_pat {|^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$|} in
    Option.try_with (fun () -> Re.exec re s) >>= fun grp ->
    let get_as_int grp i = Re.Group.get grp i |> Int.of_string in
    Some
      { id = get_as_int grp 1
      ; pos = (get_as_int grp 2, get_as_int grp 3)
      ; size = (get_as_int grp 4, get_as_int grp 5) }
end

let part_one (claims : Claim.t list) =
  let set = Hash_set.create (module IntPair) () in
  List.iter claims ~f:(fun {pos; _} -> Hash_set.add set pos)

let () =
  In_channel.with_file "input.txt" ~f:(fun inc ->
      let lines = In_channel.input_lines inc in
      let claims =
        List.map lines ~f:(fun line -> Option.value_exn (Claim.parse line))
      in
      part_one claims ; () )
