open Core

type t =
  { id : int
  ; pos : Int_pair.t
  ; size : Int_pair.t }

let parse s =
  let open Option.Monad_infix in
  let re = Re.Perl.compile_pat {|^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$|} in
  Option.try_with (fun () -> Re.exec re s) >>= fun grp ->
  let get_as_int grp i = Re.Group.get grp i |> Int.of_string in
  Some
    { id = get_as_int grp 1
    ; pos = (get_as_int grp 2, get_as_int grp 3)
    ; size = (get_as_int grp 4, get_as_int grp 5) }

let id t = t.id

let iter {pos = (start_x, start_y); size = (w, h); _} ~f =
  let max_x = start_x + w in
  let max_y = start_y + h in
  let rec loop (x, y) =
    if y = max_y then ()
    else if x = max_x then loop (start_x, y + 1)
    else (
      f (x, y) ;
      loop (x + 1, y) )
  in
  loop (start_x, start_y)

let for_all {pos = (start_x, start_y); size = (w, h); _} ~f =
  let max_x = start_x + w in
  let max_y = start_y + h in
  let rec loop (x, y) ac =
    if y = max_y then ac
    else if x = max_x then loop (start_x, y + 1) ac
    else
      let ac = ac && f (x, y) in
      if ac then loop (x + 1, y) ac else false
  in
  loop (start_x, start_y) true
