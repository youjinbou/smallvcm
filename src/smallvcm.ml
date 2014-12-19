open Utils
open Config

let get_num_procs () = 1 (* fixme *)

let get_iter, succ_iter =
  let i = ref 0 in
  let get () =
      let r = !i in
      r
    and incr () =
      incr i;
  in get, incr
    

let render aConfig aSetup =
  (* Create 1 renderer per thread *)
  let renderers = 
    Array.init aConfig.mNumThreads
               (fun i -> Config.createRenderer aConfig aSetup (aConfig.mBaseSeed + i))
  in
  let startT = Sys.time () in
  (* Rendering loop, when we have any time limit, use time-based loop,
    // otherwise go with required iterations *)
  (* Time based loop *)
  let rec timed_loop id renderer iter =
    let time = Sys.time () in
    if time < startT +. aConfig.mMaxTime
    then 
      let () = renderer#runIteration iter in
      timed_loop id renderer @@ succ iter
    else iter, time
                                  
  (* Iterations based loop *)
  and iter_loop id renderer iter =
(*    let get_iter () = iter
    and succ_iter () = () in *)
    if get_iter () < aConfig.mIterations
    then (
      Printf.printf "thread %d new iteration\n" id;
      flush stdout;
      succ_iter ();
      renderer#runIteration @@ get_iter ();
      iter_loop id renderer @@ succ iter
    ) else iter, Sys.time ()
  in
  let res, endT =
    let result = Array.make aConfig.mNumThreads (0,0.0) in
    let task_f =
      if aConfig.mMaxTime > 0.0
      then timed_loop
      else iter_loop in
    let task rank renderer iter () =
      let id = Thread.id @@ Thread.self () in
      Printf.printf "thread %d start\n" id;
      Thread.yield ();
      result.(rank) <- task_f id renderer iter;
    in
    if aConfig.mNumThreads > 1 then
      let ths = Array.mapi (fun i r -> Thread.create (task i r 0) ()) renderers in
      let res = Array.mapi (fun i t -> Thread.join t; Printf.printf "thread %d done\n" i) ths in
      res, Sys.time ()
    else
      let res = Array.init 1 (fun i -> task i renderers.(0) 0 ()) in
      res, Sys.time ()
  in
    (* Accumulate from all renderers into a common framebuffer *)
    (*  usedRenderers = 0; *)

    (* With very low number of iterations and high number of threads
     * not all created renderers had to have been used.
     * Those must not participate in accumulation. *)
  let buffer, usedRenderers =
    let accum (buff, count) renderer =
      match buff, renderer#wasUsed with
      | None, true   -> Some renderer#getFramebuffer, succ count
      | Some b, true -> Framebuffer.add b renderer#getFramebuffer; buff, succ count
      | b, false     -> b, count
    in  Array.fold_left accum (None, 0) renderers
  in
  Printf.printf "used renderers : %d\n" usedRenderers;
  match buffer with
    Some buffer ->
    (* Scale framebuffer by the number of used renderers *)
    Framebuffer.scale buffer (1. /. float usedRenderers);
    {aConfig with mFramebuffer = buffer}, (get_iter ()), endT -. startT
  | None -> assert false

(*////////////////////////////////////////////////////////////////////////
// Generates index.html with all scene-algorithm combinations. *)

let fullReport aConfig =
  failwith "unsupported yet"

(*
let nfullReport aConfig =
  (* Make a local copy of config *)
  let config = {
    aConfig with
    mSetup = default.mSetup;
  } in
  
  (* Setup html writer *)
  let  html_writer = HtmlWriter.make "index.html" (List.length Config.Algorithm.each) in
  let open HtmlWriter in
  writeHeader html_writer;

  let sceneConfigs =
    let open Scene in
    [|
      [ GlossyFloor ; SmallMirrorSphere ; SmallGlassSphere ; LightSun ];
      [ GlossyFloor ; LargeMirrorSphere ; LightCeiling ];
      [ GlossyFloor ; SmallMirrorSphere ; SmallGlassSphere ; LightPoint ];
      [ GlossyFloor ; SmallMirrorSphere ; SmallGlassSphere ; LightBackground ];
     |] in

  let configCount = Array.length sceneConfigs in
  (* Quite subjective evaluation whether given algorithm is
   * for a given scene good, poor, or neutral. *)
  let goodAlgorithms =
    let open Config.Algorithm in
    [|
      [VertexConnectionMerging;BidirectionalPhotonMapping];
      [VertexConnectionMerging;BidirectionalPhotonMapping];
      [VertexConnectionMerging;BidirectionalPhotonMapping];
      [VertexConnectionMerging;BidirectionalPathTracing]
     |]
  and poorAlgorithms =
    let open Config.Algorithm in
    [|
      [BidirectionalPathTracing];
      [BidirectionalPathTracing;ProgressivePhotonMapping];
      [ProgressivePhotonMapping];
      [BidirectionalPhotonMapping;ProgressivePhotonMapping];
     |]
  in

    let startTime = Sys.time () in

    let open Printf in

    for sceneID = 0 to pred configCount do
      let scene = Scene.cornellBox (Config.resolution config) sceneConfigs.(sceneID) |>
                    Scene.buildSphere in
      addScene html_writer scene.mSceneName;
      printf "Scene: %s\n" scene.mSceneName;

      let acc splitFilesBordersAcronyms algID =
        let setup = {mScene = scene; mAlgorithm = algID} in
        let config = {
          config with mSetup = Setup setup
        } in
        
        printf "Running %s...\n" (Config.Algorithm.name algID);
        flush stdout;
        let config, numIterations, time = render config setup in
        printf "done in %.2f s\n" time;

        let filename =
          Config.defaultFilename sceneConfigs.(sceneID)
                                                setup.mScene setup.mAlgorithm 
        in
        Framebuffer.saveBMP config.mFramebuffer ~aGamma:2.2 filename;

        (* Add thumbnail of the method *)
        let bcolor = 
          if List.exists (fun x -> x = algID) poorAlgorithms.(sceneID)
          then Red
          else if List.exists (fun x -> x = algID) goodAlgorithms.(sceneID)
          then Green 
          else None in
        
        addRendering html_writer ~aBorderColor:bcolor ~aOtherInfo:(makeMessage "<br/>Iterations: %d" numIterations) (Config.Algorithm.name setup.mAlgorithm) filename time;

        if algID >= ProgressivePhotonMapping 
        then
          (filename, bcolor, Config.Algorithm.acronym algID)::splitFilesBordersAcronyms
        else splitFilesBordersAcronyms
      in
      (* Filename and border color for images in four-way split *)
      let splitFilesBordersAcronyms = List.fold_left acc [] Config.Algorithm.each in
      addFourWaySplit html_writer splitFilesBordersAcronyms (V2i.get (Config.resolution config) 0)
    done;

    close html_writer;

    let endTime = Sys.time () in
    printf "Whole run took %.2f s\n" (endTime -. startTime)
    *)

let main () =
  let open Printf in

    (* Setups config based on command line *)
    let config = parseCommandLine () in

    (* If number of threads is invalid, set 1 thread per processor *)
    let config = 
      if config.mNumThreads <= 0
      then { config with mNumThreads = max 1 (get_num_procs ()) } 
      else config in

    match config.mSetup with
    | FullReport   -> fullReport config
    | Setup setup  ->

       (* When some error has been encountered, exits *)
       (*       if config.mScene = NULL
        return 1; *)

    (* Sets up framebuffer and number of threads *)

    (* Prints what we are doing *)
    printf "Scene:   %s\n" (Scene.sceneName setup.mScene);
    if config.mMaxTime > 0.
    then printf "Target:  %g seconds render time\n" config.mMaxTime
    else printf "Target:  %d iteration(s)\n" config.mIterations;

    (* Renders the image *)
    printf "Running: %s...\n" (Algorithm.name setup.mAlgorithm);
    flush stdout;
    let config, iter, time = render config setup in
    printf "done in %.2f s\n" time;

    (* Saves the image *)
    printf "Saving: %s... \n" config.mOutputName;
    flush stdout;
    let extension = String.sub config.mOutputName (String.length config.mOutputName - 3) 3 in
    match extension with
      "bmp" -> Framebuffer.saveBMP ~aGamma:2.2 config.mFramebuffer config.mOutputName
    | "hdr" -> Framebuffer.saveHDR config.mFramebuffer config.mOutputName
    | _     -> printf "Used unknown extension %s\n" extension

let _ = main ()
