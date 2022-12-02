open Core

type t =
  { claims : Claim.t list
  ; occupants : (Int_pair.t, int) Hashtbl.t }

let occupancy (claims : Claim.t list) =
  let occupants = Hashtbl.create (module Int_pair) ~size:1_000_000 in
  List.iter claims ~f:(fun claim ->
      Claim.iter claim ~f:(fun point ->
          Hashtbl.update occupants point ~f:(fun maybe_n ->
              1 + Option.value maybe_n ~default:0 ) ) ) ;
  occupants

let make claims = {claims; occupants = occupancy claims}

let find_claim t ~f = List.find t.claims ~f

let count_occupants t ~f = Hashtbl.count t.occupants ~f

let occupants_at t point = Hashtbl.find t.occupants point
