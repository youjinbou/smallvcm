open Utils

module type T =
sig
  type t
  val intersect : t -> Ray.t -> Isect.t -> Isect.t option
  val intersectP : t -> Ray.t -> Isect.t -> Isect.t option
  val growBB : t -> V.t * V.t -> V.t * V.t
end

module Triangle :
sig
  type t = { p : V.t * V.t * V.t; matID : int; mNormal : V.t; }
  val make : V.t -> V.t -> V.t -> int -> t
  val intersect : t -> Ray.t -> Isect.t -> Isect.t option
  val intersectP : t -> Ray.t -> Isect.t -> Isect.t option
  val growBB : t -> V.t * V.t -> V.t * V.t
  val dump : t -> unit
end

module Sphere :
sig
  type t = { center : V.t; radius : float; matID : int; }
  val make : V.t -> float -> int -> t
  val intersect : t -> Ray.t -> Isect.t -> Isect.t option
  val intersectP : t -> Ray.t -> Isect.t -> Isect.t option
  val growBB : t -> V.t * V.t -> V.t * V.t
end

type t = Triangle of Triangle.t | Sphere of Sphere.t | List of t list

val intersect : t -> Ray.t -> Isect.t -> Isect.t option
val intersectList : t list -> Ray.t -> Isect.t -> Isect.t option
val intersectP : t -> Ray.t -> Isect.t -> Isect.t option
val intersectListP : t list -> Ray.t -> Isect.t -> Isect.t option
val growBB : t -> V.t * V.t -> V.t * V.t
val growBBList : t list -> V.t * V.t -> V.t * V.t
