type t = {
  mSceneCenter : Utils.V.t;
  mSceneRadius : float;
  mInvSceneRadiusSqr : float;
}
val make : Utils.V.t -> float -> t
val sceneCenter : t -> Utils.V.t
val sceneRadius : t -> float
val invSceneRadiusSqr : t -> float
