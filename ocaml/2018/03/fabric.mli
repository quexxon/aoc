type t

val make : Claim.t list -> t

val find_claim : t -> f:(Claim.t -> bool) -> Claim.t option

val count_occupants : t -> f:(int -> bool) -> int

val occupants_at : t -> Int_pair.t -> int option
