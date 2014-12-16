
module type S =
sig

    type 'a t

    val data   : 'a t -> 'a array
    val make   : int -> 'a -> 'a t
    val length : 'a t -> int
    val get    : 'a t -> int -> 'a
    val resize : 'a t -> int -> unit
    val set    : 'a t -> int -> 'a -> unit
    val copy   : 'a t -> 'a t
    val blit   : 'a t -> int -> 'a t -> int -> int -> unit

end

type 'a t

val data   : 'a t -> 'a array
val make   : int -> 'a -> 'a t
val length : 'a t -> int
val get    : 'a t -> int -> 'a
val resize : 'a t -> int -> unit
val set    : 'a t -> int -> 'a -> unit
val copy   : 'a t -> 'a t
val blit   : 'a t -> int -> 'a t -> int -> int -> unit
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
