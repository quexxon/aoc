type t

val parse : string -> t option

val id : t -> int

val iter : t -> f:(Int_pair.t -> unit) -> unit

val for_all : t -> f:(Int_pair.t -> bool) -> bool
