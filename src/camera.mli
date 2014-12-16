open Utils

type t = {
  mPosition : V.t;
  mForward : V.t;
  mResolution : V2i.t;
  mRasterToWorld : M.t;
  mWorldToRaster : M.t;
  mImagePlaneDist : float;
}
val position : t -> V.t
val forward : t -> V.t
val resolution : t -> V2i.t
val rasterToWorldMatrix : t -> M.t
val worldToRasterMatrix : t -> M.t
val imagePlaneDist : t -> float
val perspective : float -> float -> float -> M.t
val make : V.t -> V.t -> V.t -> V2i.t -> float -> t
val rasterToIndex : t -> V2i.t -> int
val indexToRaster : t -> int -> V2i.t
val rasterToWorld : t -> V2f.t -> V.t
val worldToRaster : t -> V.t -> V2i.t
val checkRaster : t -> V2i.t -> bool
val generateRay : t -> V2f.t -> Ray.t
