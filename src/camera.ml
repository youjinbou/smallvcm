open Utils

type t = {
    mPosition : V.t;
    mForward  : V.t;
    mResolution : V2i.t;
    mRasterToWorld : M.t;
    mWorldToRaster : M.t;
    mImagePlaneDist : float;
}

let position self = self.mPosition
let forward self = self.mForward
let resolution self = self.mResolution
let rasterToWorldMatrix self = self.mRasterToWorld
let worldToRasterMatrix self = self.mWorldToRaster
let imagePlaneDist self = self.mImagePlaneDist

let v4_of_v3 v w =
  let x, y, z = tuple_of_v3 v in
  V4.of_tuple (x,y,z,w)

let perspective aFov aNear aFar =
  (* Camera points towards -z.  0 < near < far.
     Matrix maps z range [-near, -far] to [-1, 1], after homogeneous division. *)
  let f = 1. /. (tan (aFov *. pi /. 360.0))
  and d = 1. /. (aNear -. aFar) in
  let r = M.null ()
  and c0 = V4.of_tuple (f, 0.0, 0.0, 0.0)
  and c1 = V4.of_tuple (0.0, -.f, 0.0, 0.0)
  and c2 = V4.of_tuple (0.0, 0.0, (aNear +. aFar) *. d, -1.0)
  and c3 = V4.of_tuple (0.0, 0.0, 2.0 *. aNear *. aFar *. d, 0.0) in
  M.setcol r 0 c0;
  M.setcol r 1 c1;
  M.setcol r 2 c2;
  M.setcol r 3 c3;
  r

let make aPosition aForward aUp aResolution (aHorizontalFOV : float) =
  let resX, resY = (fun (x,y) -> float x, float y) (V2i.to_tuple aResolution) in
  let forward  = V.normalize aForward in
  let nforward = V.opp forward in
  let up       = V.normalize @@ cross aUp nforward in
  let left     = cross nforward up in

  let  pos = v3_of_tuple (
                 V.dot up aPosition,
                 V.dot left aPosition,
                 V.dot nforward aPosition) in
(*
  dump_v "aPosition" aPosition;
  dump_v "forward" forward;
  dump_v "left" left;
  dump_v "up"  up;
  dump_v "pos" pos;
 *)
  let worldToCamera = M.identity () in
  M.setrow worldToCamera 0 (v4_of_v3 up       (-.(V.get pos 0)));
  M.setrow worldToCamera 1 (v4_of_v3 left     (-.(V.get pos 1)));
  M.setrow worldToCamera 2 (v4_of_v3 nforward (-.(V.get pos 2)));
(*  dump_m "worldToCamera" worldToCamera; 
  prerr_newline (); *)
  let perspective    = perspective aHorizontalFOV  0.1 10000. in
  let worldToNScreen = M.mul perspective worldToCamera in
  let nscreenToWorld = M.inverse worldToNScreen in
(*
  dump_m "perspective" perspective;
  prerr_newline ();
  dump_m "worldToNScreen" worldToNScreen;
  prerr_newline ();
  dump_m "nscreenToWorld" nscreenToWorld;
  prerr_newline ();
 *)
  let tanHalfAngle   = tan (aHorizontalFOV *. pi /. 360.) in
  let translation    = M.translation 1. 1. 0.0 
  and scaling        = M.scaling (resX *. 0.5) (resY *. 0.5) 0.0 
  and invtranslation = M.translation (-1.) (-1.) 0.0 
  and invscaling     = M.scaling (2. /. resX) (2. /. resY) 0.0 in
  let mWorldToRaster =
    M.mul scaling (M.mul translation worldToNScreen)
  and mRasterToWorld  = 
    M.mul nscreenToWorld (M.mul invtranslation invscaling)
  in
(*
  dump_m "mWorldToRaster" mWorldToRaster; prerr_newline ();
  dump_m "mRasterToWorld" mRasterToWorld; prerr_newline ();
 *)
  {
    mPosition       = aPosition;
    mForward        = forward;
    mResolution     = aResolution;
    mRasterToWorld;
    mWorldToRaster;
    mImagePlaneDist = resX /. (2. *. tanHalfAngle);
  }

let rasterToIndex camera aPixelCoords =
  (V2i.get aPixelCoords 0) + (V2i.get aPixelCoords 1) * (V2i.get camera.mResolution 0)

let indexToRaster camera aPixelIndex =
  let y = ifloor ((float aPixelIndex) /. (float @@ V2i.get camera.mResolution 0)) in
  let x = aPixelIndex - y * (V2i.get camera.mResolution 0) in
  V2i.of_tuple (x, y)

let rasterToWorld camera aRasterXY : V.t =
  m_apply3 camera.mRasterToWorld @@ v3_of_v2 aRasterXY 0.0

let worldToRaster camera aWorldPos =
  let temp = m_apply3 camera.mWorldToRaster aWorldPos in
  v2i_of_v3 temp

(* returns false when raster position is outside screen space *)
let checkRaster camera aRasterPos =
  let x, y = V2i.to_tuple aRasterPos in
  x >= 0 && y >= 0 &&
    x < (V2i.get camera.mResolution 0) && y < (V2i.get camera.mResolution 1)
                                               
let generateRay camera aRasterXY =
  let worldRaster = rasterToWorld camera aRasterXY in
(*  dump_v "worldRaster" worldRaster; *)
  let open Ray in
  {org = camera.mPosition; dir = (V.normalize (V.sub worldRaster camera.mPosition)); tmin = 0.0 }
