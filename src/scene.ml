open Utils

type boxMask =
  | LightCeiling
  | LightSun
  | LightPoint
  | LightBackground
  | LargeMirrorSphere
  | LargeGlassSphere
  | SmallMirrorSphere
  | SmallGlassSphere
  | GlossyFloor
(*
  | BothSmallSpheres  (* SmallMirrorSphere && SmallGlassSphere *)
  | BothLargeSpheres  (* LargeMirrorSphere && LargeGlassSphere *)
  | Default           (* LightCeiling && BothSmallSpheres *)
 *)
(* special patterns :
SmallMirrorSphere && SmallGlassSphere 
LargeMirrorSphere && LargeGlassSphere
 *)

module IntMap = Map.Make(struct type t = int let compare x y = x - y end) 

type 'a t = {
    mGeometry:       Geometry.t;
    mCamera:         Camera.t;
    mMaterials:      Material.t array;
    mLights:         Light.t array;
    mMaterial2Light: int IntMap.t;
    mSphere:         'a;
    mBackground:     Light.BackgroundLight.t option;
    mSceneName:      string;
    mSceneAcronym:   string;
}

let geometry self = self.mGeometry
let camera self = self.mCamera
let materials self = self.mMaterials
let lights self = self.mLights
let material2Light self = self.mMaterial2Light
let sphere self = self.mSphere
let background self = self.mBackground
let sceneName self = self.mSceneName
let sceneAcronym self = self.mSceneAcronym

let buildSphere scene =
  let bbs = V.make 1e36, V.make (-1e36) in
  let bboxMin, bboxMax = Geometry.growBB scene.mGeometry bbs in
  let radius2 = lenSqr (V.sub bboxMax bboxMin) in
  let mSphere =
    SceneSphere.make (V.scale (V.add bboxMax bboxMin) 0.5) ((sqrt radius2) *. 0.5) in
  Printf.fprintf stderr "buildSphere : bbmin = %a bbmax = %a radius = %.03e\n" pprintf_v bboxMin pprintf_v bboxMax radius2;
  { scene with mSphere }

let (&) l x = List.exists (fun y -> y = x) l

let intersect self aRay oResult =
  let open Isect in
  match Geometry.intersect self.mGeometry aRay oResult with
    Some oResult -> 
    let lightID =
      try 
        IntMap.find (matID oResult) self.mMaterial2Light
    with Not_found -> -1
    in
    Some { oResult with lightID }
  | None -> None

let occluded self aPoint aDir aTMax =
  let ray = Ray.make aPoint aDir 0.0 in
  let isect = Isect.make (aTMax -. 2.*.eps_ray) 0 0 (V.null ()) in
  match Geometry.intersectP self.mGeometry ray isect with
    None -> false
  | _    -> true

let getMaterial self aMaterialIdx =
  self.mMaterials.(aMaterialIdx)

let getMaterialCount self =
  Array.length self.mMaterials

let getLightPtr self aLightIdx =
  let aLightIdx = min aLightIdx ((Array.length self.mLights) - 1) in
  self.mLights.(aLightIdx)

let getLightCount self =
  Array.length self.mLights

let getBackground self =
  self.mBackground

let getSceneName aBoxMask =
  (* Floor type *)
  ("", "")
  |> (fun (name,acronym) ->
      if aBoxMask & GlossyFloor
      then name ^ "glossy ", acronym ^ "g"
      else name, acronym)
  (* Box content *)
  |> (fun (name,acronym) ->
      if aBoxMask & SmallMirrorSphere
      then if aBoxMask & SmallGlassSphere
           then name ^ "small spheres ", acronym ^ "bs"
           else name ^ "small mirror sphere ", acronym ^ "sm"
      else if aBoxMask & SmallGlassSphere
      then name ^ "small glass sphere ", acronym ^ "sg"
      else if aBoxMask & LargeMirrorSphere
      then name ^ "large mirror sphere ", acronym ^ "lm"
      else if aBoxMask & LargeGlassSphere
      then name ^ "large glass sphere ", acronym ^ "lg"
      else name ^ "empty", acronym ^ "e")
  |> (fun (name, acronym) ->
      name, acronym ^ "_")
  (* Lighting *)
  |> (fun (name, acronym) ->
      if aBoxMask & LightCeiling
      then name ^ "+ ceiling (area) ", acronym ^ "c"
      else if aBoxMask & LightSun
      then name ^ "+ sun (directional) ", acronym ^ "s"
      else if aBoxMask & LightPoint
      then name ^ "+ point ", acronym ^ "p"
      else if aBoxMask & LightBackground
      then name ^ "+ background (env. lighting) ", acronym ^ "b"
      else name, acronym)
         
let cornellBox aResolution aBoxMask =
  let open Geometry in
  let mSceneName, mSceneAcronym = getSceneName aBoxMask in
  let aBoxMask =
    if (aBoxMask & LargeMirrorSphere) && (aBoxMask & LargeGlassSphere)
    then (
      prerr_endline "Cannot have both large balls, using mirror";
      List.filter (fun i -> i <> LargeGlassSphere) aBoxMask
    ) else aBoxMask in

  let light_ceiling    = (aBoxMask & LightCeiling)
  and light_sun        = (aBoxMask & LightSun)
  and light_point      = (aBoxMask & LightPoint)
  and light_background = (aBoxMask & LightBackground) in
  (* because it looks really weird with it *)
  let light_box        = not light_point in
  let mCamera = Camera.make 
                  (v3_of_tuple (-0.0439815,-4.12529,   0.222539))
                  (v3_of_tuple (0.00688625, 0.998505, -0.0542161))
                  (v3_of_tuple (3.73896e-4, 0.0542148, 0.998529))
                  aResolution
                  45.0 in
  (* Materials *)
  (* 0) light1, will only emit *)
  let light1_mat = Material.default
  (* 1) light2, will only emit *)
  and light2_mat = Material.default
  (* 2) glossy white floor *)
  and floor_mat = Material.make ~diffuse_refl:(V.make 0.1)
                           ~phong_refl:(V.make 0.7)
                           ~phong_exp:90.0 ()

  (* 3) diffuse green left wall *)
  and left_mat = Material.make ~diffuse_refl:(v3_of_tuple (0.156863, 0.803922, 0.172549)) ()

  (* 4) diffuse red right wall *)
  and right_mat = Material.make ~diffuse_refl:(v3_of_tuple (0.803922, 0.152941, 0.152941)) ()
  (* 5) diffuse white back wall *)
  and back_mat = Material.make ~diffuse_refl:(v3_of_tuple (0.803922, 0.803922, 0.803922)) ()
  (* 6) mirror ball *)
  and mirror_mat = Material.make ~mirror_refl:(V.make 1.0) ()
  (* 7) glass ball *)
  and glass_mat = Material.make ~mirror_refl:(V.make 1.0) ~ior:1.6 ()
  (* 8) diffuse blue wall (back wall for glossy floor) *)
  and blue_mat = Material.make ~diffuse_refl:(v3_of_tuple (0.156863, 0.172549, 0.803922)) ()
  in
  let mMaterials = 
    [| light1_mat; light2_mat; floor_mat; left_mat; right_mat; 
       back_mat; mirror_mat; glass_mat; blue_mat |] in
  (*//////////////////////////////////////////////////////////////////////*)
  let mkbox v1 v2 =
    let vmake a b c = v3_of_tuple (a,b,c) in
    let x1,y1,z1 = tuple_of_v3 v1
    and x2,y2,z2 = tuple_of_v3 v2 in
    [|
      vmake x1 y2 z1;
      vmake x2 y2 z1;
      vmake x2 y2 z2;
      vmake x1 y2 z2;
      vmake x1 y1 z1;
      vmake x2 y1 z1;
      vmake x2 y1 z2;
      vmake x1 y1 z2
     |] in 
  (* Cornell box *)
  let v1 = v3_of_tuple (-1.27029, -1.25549,-1.28002)
  and v2 = v3_of_tuple ( 1.28975,  1.30455, 1.28002)
  and mkwall vt a b c d s1 s2 = [
      Triangle (Triangle.make vt.(a) vt.(b) vt.(c) s1);
      Triangle (Triangle.make vt.(c) vt.(d) vt.(a) s2);
    ] in
  (* room geometry *)
  let cb = mkbox v1 v2 in
  (* lightbox geometry *)
  let lv1 = v3_of_tuple (-0.25, -0.25, 1.26002)
  and lv2 = v3_of_tuple ( 0.25,  0.25, 1.28002) in
  let lb = mkbox lv1 lv2 in
  let mGeometry =
    let floor = 
      if (aBoxMask & GlossyFloor)
      then
        (* Floor *) 
        mkwall cb 0 4 5 1 2 2
        (* Back wall *)
        @ mkwall cb 0 1 2 3 8 8
      else
        (* Floor *)
        mkwall cb 0 4 5 1 5 5
        (* Back wall *)
        @ mkwall cb 0 1 2 3 5 5
    and ceiling =
      (* Ceiling *)
      if light_ceiling && not light_box
      then
        mkwall cb 2 6 7 3 0 1
      else
        mkwall cb 2 6 7 3 5 5
    and side_walls =
      (* Left wall *)
      mkwall cb 3 7 4 0 3 3
      (* Right wall *)
      @ mkwall cb 1 5 6 2 4 4
    and central_ball =
      (* Ball - central *)
      let largeRadius = 0.8 in
      let center = V.add (V.scale (V.add (V.add cb.(0) cb.(1)) (V.add cb.(4) cb.(5))) 0.25)
                   @@ v3_of_tuple (0., 0., largeRadius) in
      if aBoxMask & LargeMirrorSphere
      then [Sphere (Sphere.make center largeRadius 6)] 
      else if aBoxMask & LargeGlassSphere
      then [Sphere (Sphere.make center largeRadius 7)] else []
    and small_balls =
      (* Balls - left and right *)
      let smallRadius = 0.5 in
      let leftWallCenter  = V.add (V.scale (V.add cb.(0) cb.(4)) 0.5) @@ v3_of_tuple (0., 0., smallRadius)
      and rightWallCenter = V.add (V.scale (V.add cb.(1) cb.(5)) 0.5) @@ v3_of_tuple (0., 0., smallRadius) in
      let xlen = (V.get rightWallCenter 0) -. (V.get leftWallCenter 0) in
      let leftBallCenter  = V.add leftWallCenter @@ v3_of_tuple (2.*.xlen/.7., 0., 0.)
      and rightBallCenter = V.sub rightWallCenter @@ v3_of_tuple (2.*.xlen/.7., 0., 0.) in
      (if aBoxMask & SmallMirrorSphere
      then [Sphere (Sphere.make leftBallCenter smallRadius 6) ] else [])
      @ (if aBoxMask & SmallGlassSphere
         then [Sphere (Sphere.make rightBallCenter smallRadius 7) ] else [])
          
    (*  ////////////////////////////////////////////////////////////////////////// *)
    (* Light box at the ceiling *) 
    and lightbox = 
      let box =
        if light_box
        then
          (* Back wall *)
          mkwall lb 2 1 0 3 5 5
          (* Left wall *)
          @ mkwall lb 4 7 3 0 5 5 (* 4 7 3 - 3 0 4 *)
          (* Right wall *)
          @ mkwall lb 6 5 1 2 5 5 (* 6 5 1 - 1 2 6 *)
          (* Front wall *)
          @ mkwall lb 4 5 6 7 5 5 (* 4 5 6 - 6 7 4 *)

        else []
      and light =   
        if(light_ceiling)
        then
          (* Floor *)
          mkwall lb 5 4 0 1 0 1
        else
          (* Floor *)
          mkwall lb 5 4 0 1 5 5 
      in
      box @ light in
    List (floor @ ceiling @ side_walls @ central_ball @ small_balls @ lightbox)
  and mLights, mBackground, mMaterial2Light =
    let open Light in
    (* ////////////////////////////////////////////////////////////////////////// *)
    (* Lights *)
    let ceiling, mats =
      if light_ceiling 
      then if not light_box
           then
             (* Without light box, whole ceiling is light *)
             let l0 = AreaLight (AreaLight.make cb.(2) cb.(6) cb.(7) (V.make 0.95492965))
             and mat0 = (0,0) in
             let l1 = AreaLight (AreaLight.make cb.(7) cb.(3) cb.(2) (V.make 0.95492965))
             and mat1 = (1,1) in
             [l0;l1], [mat0;mat1]
           else 
             (* With light box *)
             let l0 = AreaLight (AreaLight.make lb.(0) lb.(5) lb.(4) (V.make 25.03329895614464))
             and mat0 = (0,0) in
             let l1 = AreaLight (AreaLight.make lb.(5) lb.(0) lb.(1) (V.make 25.03329895614464))
             and mat1 = (1,1) in
             [l0;l1], [mat0;mat1]
      else [], []
    and sun = 
      if light_sun
      then [ DirectionalLight (DirectionalLight.make (v3_of_tuple (-1., 1.5, -1.)) (V.scale (v3_of_tuple (0.5, 0.2, 0.)) 20.))]
      else []
    and point =
      if light_point
      then [ PointLight (PointLight.make (v3_of_tuple (0.0, -0.5, 1.0)) (V.make @@ 70. *. inv_pi *. 0.25))]
      else []
    in
    let mat2light = List.fold_left (fun m (a,b) -> IntMap.add a b m) IntMap.empty mats
    and mlights = ceiling @ sun @ point in
    if light_background 
    then let background = BackgroundLight.make 1.
         in Array.of_list (mlights @ [BackgroundLight background]), Some background, mat2light
    else Array.of_list mlights, None, mat2light
  in {
    mGeometry;
    mCamera;
    mMaterials;
    mLights;
    mMaterial2Light;
    mSphere = ();
    mBackground;
    mSceneName;
    mSceneAcronym;
  }

