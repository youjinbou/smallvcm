open Utils

type t = { dist : float; matID : int; lightID : int; normal : V.t; }
val default : t
val make : float -> int -> int -> V.t -> t
val dist : t -> float
val set_dist : t -> float -> t
val matID : t -> int
val lightID : t -> int
val normal : t -> V.t

val dump : out_channel -> t -> unit
