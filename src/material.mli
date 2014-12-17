type t = {
  mDiffuseReflectance : Utils.V.t;
  mPhongReflectance : Utils.V.t;
  mPhongExponent : float;
  mMirrorReflectance : Utils.V.t;
  mIOR : float;
}
val diffuseReflectance : t -> Utils.V.t
val phongReflectance : t -> Utils.V.t
val phongExponent : t -> float
val mirrorReflectance : t -> Utils.V.t
val ior : t -> float
val default : t
val make :
  ?diffuse_refl:Utils.V.t ->
  ?phong_refl:Utils.V.t ->
  ?phong_exp:float -> ?mirror_refl:Utils.V.t -> ?ior:float -> unit -> t

val dump : out_channel -> t -> unit
