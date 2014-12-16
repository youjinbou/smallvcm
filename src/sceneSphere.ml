open Utils

type t = {
  (* Center of the scene's bounding sphere *)
  mSceneCenter : V.t;
  (* Radius of the scene's bounding sphere *)
  mSceneRadius : float;
  (* 1.f / (mSceneRadius^2) *)
  mInvSceneRadiusSqr : float;
}

let make v r = {
  mSceneCenter = v;
  mSceneRadius = r;
  mInvSceneRadiusSqr = 1. /. (r *. r);
}

let sceneCenter s = s.mSceneCenter
let sceneRadius s = s.mSceneRadius
let invSceneRadiusSqr s = s.mInvSceneRadiusSqr
