open Utils

type t = {
  mBBoxMin : V.t;
  mBBoxMax : V.t;
  mIndices : int array;
  mCellEnds : int array;
  mRadius : float;
  mRadiusSqr : float;
  mCellSize : float;
  mInvCellSize : float;
}

val bbMin : t -> V.t
val bbMax : t -> V.t
val indices : t -> int array
val cellEnds : t -> int array
val radius : t -> float
val radiusSqr : t -> float
val cellSize : t -> float
val invCellSize : t -> float
val resize : 'a array -> int -> 'a -> 'a array
val reserve : t -> int -> t
val getCellRange : t -> int -> V2i.t
val getCellIndexFromCoord : int -> V3i.t -> int
val getCellIndexFromPoint :  V.t -> float -> int -> V.t -> int


module type PARTICLE = sig
    type t
    val getPosition : t -> V.t
end

module type QUERY = sig

    module Particle : PARTICLE

    type t
    val getPosition : t -> V.t
    val process : t -> Particle.t -> unit

end

module type S =
sig
  module Particle : PARTICLE
  type query
  val build : Particle.t array -> int -> float -> t
  val process : t -> Particle.t array -> query -> unit
end

module Make : functor (Query : QUERY) -> S with module Particle = Query.Particle
                                            and type query = Query.t
