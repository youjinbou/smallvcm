open Utils

let color_add a b =
(*  let x1,y1,z1 = tuple_of_v3 a
  and x2,y2,z2 = tuple_of_v3 b in
  Printf.fprintf stderr "color add : {%e,%e,%e} + {%e,%e,%e}\n" x1 y1 z1 x2 y2 z2; *)
  V.add a b

module Make (Rng : Rng.S) = struct

  class pathtracer aScene pathLengths aSeed =
  object(self)

    inherit Renderer.abstractRenderer aScene pathLengths as super

    val mRng = Rng.make aSeed

    (* Mis power (1 for balance heuristic) *)
    method private mis aPdf = aPdf
    (* Mis weight for 2 pdfs *)
    method private  mis2 aSamplePdf aOtherPdf =
      (self#mis aSamplePdf) /. ((self#mis aSamplePdf) +. (self#mis aOtherPdf))

    method private accIntersect pathLength ray lastSpecular lightPickProb
                                lastPdfW pathWeight color =
      if pathLength < mMinPathLength
      then color
      else
        match Scene.getBackground mScene with
        | None -> color
        | Some background ->
           (* For background we cheat with the A/W suffixes,
            * and GetRadiance actually returns W instead of A *)
           let contrib, directPdfW =
             Light.BackgroundLight.getRadiance background (Scene.sphere mScene)
                                               (Ray.dir ray) (V.null ()) in
           if notZero contrib
           then (
             prerr_endline "accIntersect notZero contrib";
             let misWeight =
               if pathLength > 1 && not lastSpecular
               then self#mis2 lastPdfW (directPdfW *. lightPickProb)
               else 1.
             in color_add color @@ V.mul contrib (V.scale pathWeight misWeight)
           ) else color

    method private accLightNoReflect pathLength ray isect bsdf lastSpecular
                                     lightPickProb lastPdfW hitPoint pathWeight color =
      (* directPly hit some light, lights do not reflect *)
       prerr_string "accLightNoReflect...";
      if (Isect.lightID isect) >= 0
      then (
        if pathLength >= mMinPathLength
        then
          let light = Scene.getLightPtr mScene (Isect.lightID isect) in
          let contrib, directPdfA = Light.getRadiance light (Scene.sphere mScene)
                                                      (Ray.dir ray) hitPoint in
          if notZero contrib
          then (
            prerr_endline "notZero contrib";
            let misWeight =
              if pathLength > 1 && not lastSpecular
              then let directPdfW = pdfAtoW directPdfA (Isect.dist isect) (Bsdf.cosThetaFix bsdf) in
                   self#mis2 lastPdfW (directPdfW *. lightPickProb)
              else 1.
            in color_add color @@ V.mul contrib (V.scale pathWeight misWeight)
          ) else (
            prerr_endline "Zero contrib";
            color
          )
        else color
      ) else color

    method private nextEventEstimation bsdf pathLength lightCount lightPickProb
                                       hitPoint pathWeight color : V.t =
      prerr_endline "nextEventEstimation";
      let open Light in
      let open Bsdf in
      if Bsdf.isDelta bsdf || pathLength + 1 < mMinPathLength
      then (
        prerr_endline "isDelta bsdf || pathLength + 1 < mMinPathLength";
        color
      ) else
        let rnd = Rng.getFloat mRng in
        let lightID = int_of_float @@ rnd *. float lightCount in
        Printf.fprintf stderr "rnd = %.03e lightCount = %d\n" rnd lightCount;
        let light = Scene.getLightPtr mScene lightID in
        let radiance =
          Light.illuminate light (Scene.sphere mScene) hitPoint
                           (Rng.getVec2f mRng) true false in
        if isZero radiance.radiance
        then (
          prerr_endline @@ "Zero radiance (" ^ string_of_int lightID ^ ")";
          color
        )
        else (
          prerr_endline @@ "nonZero radiance (" ^ string_of_int lightID ^ ")";
          let factor = Bsdf.evaluate bsdf mScene radiance.oDirectionToLight in
          if isZero factor.oBSDF
          then
            let () = prerr_endline @@ "Zero BSDF" in
            color
          else
            let () = prerr_endline @@ "nonZero BSDF" in
            let weight =
              if not @@ Light.isDelta light
              then
                let contProb = Bsdf.continuationProb bsdf in
                self#mis2 (radiance.oDirectPdfW *. lightPickProb) (factor.oDirectPdfW *. contProb)
              else 1. in
            if Scene.occluded mScene hitPoint radiance.oDirectionToLight radiance.oDistance
            then (
                 prerr_endline "occluded";
                color
            ) else
              let rad = V.mul radiance.radiance factor.oBSDF in
              let k = weight *. factor.oCosThetaGen /. (lightPickProb *. radiance.oDirectPdfW) in
              let contrib = V.scale rad k in
              dump_v "radiance" radiance.radiance;
              dump_v "bsdf" factor.oBSDF;
              Printf.fprintf stderr "weight = %.03e; cos = %.03e; pdf = %.03e; k = %.03e\n" weight factor.oCosThetaGen radiance.oDirectPdfW k;

              color_add color @@ V.mul contrib pathWeight
        )

    method private randomWalk color ray isect bsdf hitPoint pathWeight cont =
      let open Bsdf in
      let rndTriplet = Rng.getVec3f mRng in
      let factor = Bsdf.sample bsdf mScene rndTriplet in
      if notZero factor.oBSDF
      then
        (* Russian roulette *)
        let contProb     = Bsdf.continuationProb bsdf in
        let lastSpecular = factor.oSampledEvent = Reflect || factor.oSampledEvent = Refract in
        let lastPdfW     = factor.oPdfW *. contProb in
        let cont pdf =
          let pathWeight = V.mul pathWeight @@ V.scale factor.oBSDF (factor.oCosThetaGen /. pdf) in
          let oRayDir = factor.oWorldDirGen in
          let ray     = Ray.make hitPoint oRayDir 0. in
          let isect   = Isect.set_dist isect 1e36 in
          cont color pathWeight ray isect lastSpecular lastPdfW
        in
        match contProb < 1., Rng.getFloat mRng > contProb with
          true, true  -> color
        | true, false -> cont lastPdfW
        | false, _    -> cont factor.oPdfW
      else color

    method runIteration aIteration =
      (* We sample lights uniformly *)
      prerr_string @@ "runIteration [" ^ string_of_int mMinPathLength ^ ","  ^ string_of_int mMaxPathLength ^ "]...";
      if Scene.getBackground mScene = None
      then prerr_endline "no background"
      else prerr_endline "with background";
      let lightCount    = Scene.getLightCount mScene in
      let lightPickProb = 1. /. (float lightCount) in
      Printf.fprintf stderr "light count = %d\n" lightCount;
      let camera = Scene.camera mScene in
      let resX, resY =
        let x,y = V2i.to_tuple @@ Camera.resolution camera in
        x,y
      in

      for y = 0 to pred resY do
        for x = 0 to pred resX do
          let pixID = y * resX + x in
          Printf.fprintf stderr "pixID = %d x = %d y = %d\n" pixID x y;
          let sample = V2f.add (V2f.of_tuple (float x, float y)) (Rng.getVec2f mRng) in
          let ray = Camera.generateRay camera sample in
          let isect = Isect.default in
          let pathWeight = V.make 1.
          and color = V.null ()
          and pathLength   = 1
          and lastSpecular = true
          and lastPdfW     = 1. in
          let rec loop pathLength color pathWeight ray isect lastSpecular lastPdfW =
            match Scene.intersect mScene ray isect with
            | None -> (* no intersection, we're done *)
               prerr_endline @@ "no intersection ("^(string_of_int pathLength) ^")";
               self#accIntersect pathLength ray lastSpecular lightPickProb lastPdfW pathWeight color
            | Some isect ->
               Printf.fprintf stderr "isect : %a\n" Isect.dump isect;
               let hitPoint = V.add (Ray.origin ray) (V.scale (Ray.dir ray) (Isect.dist isect)) in
               let isect = Isect.set_dist isect (eps_ray +. Isect.dist isect) in
               let bsdf = Bsdf.make false ray isect mScene in
               if Bsdf.isValid bsdf
               then
                 if Isect.lightID isect >= 0
                 then self#accLightNoReflect pathLength ray isect bsdf lastSpecular
                                             lightPickProb lastPdfW hitPoint pathWeight color
                 else
                   if pathLength < mMaxPathLength && Bsdf.continuationProb bsdf <> 0.
                   then (* next event estimation *)
                     let color =
                       self#nextEventEstimation bsdf pathLength lightCount lightPickProb hitPoint pathWeight color
                     in
                     self#randomWalk color ray isect bsdf hitPoint pathWeight (loop @@ succ pathLength)
                   else (
                     prerr_endline @@ "maxPathLength" ^ string_of_float (Bsdf.continuationProb bsdf);
                     color)
               else (
                 prerr_endline @@ "invalid bsdf ("^(string_of_int pathLength) ^")";
                 color)
          in
          let color = loop pathLength color pathWeight ray isect lastSpecular lastPdfW in
          if notZero color then
            let () = dump_v "adding color" color in
            Framebuffer.addColor mFramebuffer (v2i_of_v2f sample) color;
        done
      done;
      mIterations <- succ mIterations

  end (* class pathTracer *)

  let renderer aScene pathLengths aSeed =
    (new pathtracer aScene pathLengths aSeed :> Renderer.abstractRenderer)

end (* module Make *)
