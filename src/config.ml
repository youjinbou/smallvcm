open Utils

module Algorithm = struct

  type t =
    | EyeLight
    | PathTracing
    | LightTracing
    | ProgressivePhotonMapping
    | BidirectionalPhotonMapping
    | BidirectionalPathTracing
    | VertexConnectionMerging

  let each = [
      EyeLight
    ; PathTracing
    ; LightTracing
    ; ProgressivePhotonMapping
    ; BidirectionalPhotonMapping
    ; BidirectionalPathTracing
    ; VertexConnectionMerging
  ]

  let name = function
    | EyeLight                   -> "Eye Light"
    | PathTracing                -> "Path Tracing"
    | LightTracing               -> "Light Tracing"
    | ProgressivePhotonMapping   -> "Progressive Photon Mapping"
    | BidirectionalPhotonMapping -> "Bidirectional Photon Mapping"
    | BidirectionalPathTracing   -> "Bidirectional Path Tracing"
    | VertexConnectionMerging    -> "Vertex Connection Merging"

  let acronym = function
    | EyeLight                   -> "el"
    | PathTracing                -> "pt"
    | LightTracing               -> "lt"
    | ProgressivePhotonMapping   -> "ppm"
    | BidirectionalPhotonMapping -> "bpm"
    | BidirectionalPathTracing   -> "bpt"
    | VertexConnectionMerging    -> "vcm"

  let default = VertexConnectionMerging

  let of_acronym = function
    | "el"  -> EyeLight
    | "pt"  -> PathTracing
    | "lt"  -> LightTracing
    | "ppm" -> ProgressivePhotonMapping
    | "bpm" -> BidirectionalPhotonMapping
    | "bpt" -> BidirectionalPathTracing
    | "vcm" -> VertexConnectionMerging
    | _ -> failwith "invalid algorithm"

  let of_int = function
    | 0 -> EyeLight
    | 1 -> PathTracing
    | 2 -> LightTracing
    | 3 -> ProgressivePhotonMapping
    | 4 -> BidirectionalPhotonMapping
    | 5 -> BidirectionalPathTracing
    | 6 -> VertexConnectionMerging
    | _ -> failwith "invalid algorithm"

end

type 'a render = {
  mScene         : 'a;
  mAlgorithm     : Algorithm.t;
}

type 'a setup =
    FullReport
  | Setup of 'a render

type ('a,'f) t = {
  mIterations    : int;
  mMaxTime       : float;
  mRadiusFactor  : float;
  mRadiusAlpha   : float;
  mFramebuffer   : 'f;
  mNumThreads    : int;
  mBaseSeed      : int;
  mMaxPathLength : int;
  mMinPathLength : int;
  mOutputName    : string;
  mResolution    : int * int;
  mRng           : Rng.t;
  mSetup         : 'a setup;
}

let resolution config = V2i.of_tuple config.mResolution

(* Scene configurations *)
let sceneConfigs =
  let open Scene in [|
    [ GlossyFloor ; SmallMirrorSphere ; SmallGlassSphere ; LightSun ];
    [ GlossyFloor ; LargeMirrorSphere ; LightCeiling ];
    [ GlossyFloor ; SmallMirrorSphere ; SmallGlassSphere ; LightPoint ];
    [ GlossyFloor ; SmallMirrorSphere ; SmallGlassSphere ; LightBackground ];
   |]

let default_setup =  {
  mScene         = sceneConfigs.(0);
  mAlgorithm     = Algorithm.VertexConnectionMerging;
}

let default = {
    mIterations    = 1;
    mMaxTime       = -1.;
    mOutputName    = "";
    mNumThreads    = 0;
    mBaseSeed      = 1234;
    mMaxPathLength = 10;
    mMinPathLength = 0;
    mResolution    = (512, 512);
    mRadiusFactor  = 0.003;
    mRadiusAlpha   = 0.75;
    mFramebuffer   = ();
    mRng           = Rng.default;
    mSetup         = Setup default_setup
}

(* Utility function, essentially a renderer factory *)
let createRenderer (aConfig : ('a,'f) t) aSetup aSeed =
  debug "createRenderer : [%d,%d]\n" aConfig.mMinPathLength aConfig.mMaxPathLength;
  let open Algorithm in
  let scene = aSetup.mScene in
  let pathLengths = aConfig.mMinPathLength, aConfig.mMaxPathLength in
  let module Rng = (val (Rng.getModule aConfig.mRng) : Rng.S) in
  match aSetup.mAlgorithm with
  | EyeLight                   -> let module M = Eyelight.Make(Rng) in
                                  M.renderer scene pathLengths aSeed
  | PathTracing                -> let module M = Pathtracer.Make(Rng) in
                                  M.renderer scene pathLengths aSeed
  | LightTracing               -> let module M = VertexCM.Make(Rng) in
                                  M.renderer scene pathLengths VertexCM.LightTrace
                                             aConfig.mRadiusFactor aConfig.mRadiusAlpha aSeed
  | ProgressivePhotonMapping   -> let module M = VertexCM.Make(Rng) in
                                  M.renderer scene pathLengths VertexCM.Ppm
                                            aConfig.mRadiusFactor aConfig.mRadiusAlpha aSeed
  | BidirectionalPhotonMapping -> let module M = VertexCM.Make(Rng) in
                                  M.renderer scene pathLengths VertexCM.Bpm
                                            aConfig.mRadiusFactor aConfig.mRadiusAlpha aSeed
  | BidirectionalPathTracing   -> let module M = VertexCM.Make(Rng) in
                                  M.renderer scene pathLengths VertexCM.Bpt
                                            aConfig.mRadiusFactor aConfig.mRadiusAlpha aSeed
  | VertexConnectionMerging    -> let module M = VertexCM.Make(Rng) in
                                  M.renderer scene pathLengths VertexCM.Vcm
                                            aConfig.mRadiusFactor aConfig.mRadiusAlpha aSeed

let defaultFilename aSceneConfig (aScene : 'a Scene.t) aAlgorithm aRng =
    (if
        List.exists (fun x -> x = Scene.GlossyFloor) aSceneConfig
      then "g" else "") ^
      (Scene.sceneAcronym aScene) ^ "_" ^ Algorithm.acronym aAlgorithm ^ "_" ^ Rng.acronym aRng ^ ".bmp"

let report_desc =
  " Renders all scenes using all algorithms and generates an index.html file"
  ^ "that displays all images. Obeys the -t and -i options, ignores the rest.\n"
  ^ "Recommended usage: --report -i 1   (fastest preview)\n"
  ^ "Recommended usage: --report -t 10  (takes 5.5 mins)\n"
  ^ "Recommended usage: --report -t 60  (takes 30 mins)\n"
  ^ "Note: Time (-t) takes precedence over iterations (-i) if both are defined."

let usageMsg pname = pname ^ ": small implementation of Vertex Connection Merging (VCM) Rendering algorithm"


let parseCommandLine () =
  let open Arg in
  let config = default in
  let conf = ref config in
  let algo = ref Algorithm.default
  and sceneConfig = ref sceneConfigs.(0)
  and full_report = ref (config.mSetup = FullReport) in
  let set_algo i =
    try
      algo := Algorithm.of_acronym i
    with Failure m -> raise (Bad m)
  and set_scene i =
    if i < Array.length sceneConfigs
    then sceneConfig := sceneConfigs.(i)
    else raise (Bad "invalid scene")
  and set_rng i = conf := { !conf with mRng = Rng.of_acronym i}
  and set_cores i = conf := { !conf with mNumThreads = i }
  in
  let cmdline_specs =
    let module A = Algorithm in
    align [
        "-a", String set_algo,
        " Selects the rendering algorithm\n"
         ^ "(availables:\n" ^
           (List.fold_left (fun s i -> s ^ " " ^ A.acronym i ^ " = " ^ A.name i ^ "\n") "" A.each)
            ^ "default: " ^ A.acronym default_setup.mAlgorithm ^ " )";
        "-c", Int set_cores,
        " Selects the number of threads (default "^ string_of_int default.mNumThreads ^")";
        "-s", Int set_scene,
        " Selects the scene (default 0)";
        "-t", Int (fun i -> conf := { !conf with mMaxTime = float i }),
        " Number of seconds to run the algorithm";
        "-i", Int (fun i -> conf := { !conf with mIterations = i }),
        " Number of iterations to run the algorithm (default 1)";
        "-o", String (fun s -> conf := { !conf with mOutputName = s }),
        " User specified output name, with extension .bmp or .hdr (default .bmp)";
        "-x", Int (fun x ->
                   let _, resY = !conf.mResolution in
                   conf := { !conf with mResolution = (x,resY) }),
        " Horizontal resolution";
        "-y", Int (fun y ->
                   let resX,_ = !conf.mResolution in
                   conf := { !conf with mResolution = (resX,y) }),
        " Vertical resolution";
        "-r", String set_rng,
        " Selects the Random Number Generator\n"
         ^ "(availables:\n" ^
           (List.fold_left (fun s i -> s ^ " " ^ Rng.acronym i ^ " = " ^ Rng.name i ^ "\n") "" Rng.each)
            ^ "default: " ^ Rng.acronym default.mRng ^ " )";
        "--report", Set full_report,
        report_desc
      ]
  in
  parse cmdline_specs (fun s -> raise (Bad ("unknown parameter '"^s^"'"))) (usageMsg Sys.argv.(0));
  if !full_report
  then { !conf with mSetup = FullReport }
  else
    let r =
      let scene = Scene.cornellBox (resolution !conf) !sceneConfig in
      let scene = Scene.buildSphere scene
      in { mScene = scene; mAlgorithm = !algo } in
    let name =
      match (!conf).mOutputName with
        "" -> defaultFilename !sceneConfig r.mScene !algo (!conf).mRng
      | n  -> if String.length n > 4
                 && (Filename.check_suffix n ".bmp"
                     || Filename.check_suffix n ".hdr")
              then n
              else n ^ ".bmp"
    in
    { !conf with mOutputName = name; mSetup = Setup r }
