open Utils

type t = { org : V.t; dir : V.t; tmin : float; }
val make : V.t -> V.t -> float -> t
val origin : t -> V.t
val dir : t -> V.t
val tmin : t -> float

val dump : out_channel -> t -> unit
