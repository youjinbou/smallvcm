type t = {
  mColor : Utils.V.t array;
  mResolution : Utils.V2i.t;
  mResX : int;
  mResY : int;
}
val addColor : t -> Utils.V2i.t -> Utils.V.t -> unit
val clone : t -> t
val update_all : (int -> 'a -> 'a) -> 'a array -> unit
val clear : t -> unit
val setup : Utils.V2i.t -> t
val add : t -> t -> unit
val scale : t -> Utils.V.scalar -> unit
val totalLuminance : t -> float
val savePPM : t -> ?aGamma:float -> string -> unit
val savePFM : t -> string -> unit

type uint32 = int32
type int16 = int

val saveBMP : t -> ?aGamma:float -> string -> unit
val saveHDR : t -> string -> unit
