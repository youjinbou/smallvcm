
open Utils

(*//////////////////////////////////////////////////////////////////////////////
 * A NOTE ON PATH MIS WEIGHT EVALUATION
 *//////////////////////////////////////////////////////////////////////////////
 *
 * We compute path MIS weights iteratively as we trace the light and eye
 * sub-paths. We cache three floating points quantities at each sub-path vertex:
 *
 *   dVCM  dVC  dVM
 *
 * These quantities represent partial weights associated with the sub-path. When
 * we connect or merge one vertex to another, we use these quantities to quickly
 * evaluate the MIS weight for the full path we have constructed. This scheme is
 * presented in the technical report
 *
 *   "Implementing Vertex Connection and Merging"
 *   http://www.iliyan.com/publications/ImplementingVCM
 *
 * The MIS code in the VertexCM class references the corresponding equations in
 * the report in the form
 *
 *   [tech. rep. (##)]
 *
 * where ## is the equation number. 
 *)


type algorithmType =
  (* light vertices contribute to camera,
   * No MIS weights (dVCM, dVM, dVC all ignored) *)
  | LightTrace
  (* Camera and light vertices merged on first non-specular surface from camera.
   * Cannot handle mixed specular + non-specular materials.
   * No MIS weights (dVCM, dVM, dVC all ignored) *)
  | Ppm
  (* Camera and light vertices merged on along full path.
   * dVCM and dVM used for MIS *)
  | Bpm
  (* Standard bidirectional path tracing
   * dVCM and dVC used for MIS *)
  | Bpt
  (* Vertex connection and mering
   * dVCM, dVM, and dVC used for MIS *)
  | Vcm

module Make (Rng : Rng.S) = struct

  (* The sole point of this structure is to make carrying around the ray baggage easier. *)
  type subPathState = {
    mOrigin        : V.t;  (* Path origin *)
    mDirection     : V.t;  (* Where to go next *)
    mThroughput    : V.t;  (* Path throughput *)
    mPathLength    : int;    (* Number of path segments, including this *)
    mIsFiniteLight : bool;   (* Just generate by finite light *)
    mSpecularPath  : bool;   (* All scattering events so far were specular *)

    dVCM : float;   (* MIS quantity used for vertex connection and merging *)
    dVC  : float;   (* MIS quantity used for vertex connection *)
    dVM  : float;   (* MIS quantity used for vertex merging *)
  }

  let dumpPathState ppf s = 
    Printf.fprintf ppf "{ org = %a; dir = %a; throughput = %a; pathLength = %d;dVCM = %.03e; dVC = %.03e; dVM = %.03e }" pprintf_v s.mOrigin pprintf_v s.mDirection  pprintf_v s.mThroughput s.mPathLength s.dVCM s.dVC s.dVM

  module PathVertex = struct
    (* Path vertex, used for merging and connection *)
    type t = {
      tFromLight   : bool;
      mHitpoint    : V.t;    (* Position of the vertex *)
      mThroughput  : V.t;    (* Path throughput (including emission) *)
      mPathLength  : int;    (* Number of segments between source and vertex *)

      (* Stores all required local information, including incoming direction. *)
      mBsdf        : Bsdf.t ;

      dVCM : float;   (* MIS quantity used for vertex connection and merging *)
      dVC  : float;   (* MIS quantity used for vertex connection *)
      dVM  : float;   (* MIS quantity used for vertex merging *)
    }
    (* Used by HashGrid *)
    let getPosition self =
      self.mHitpoint

  end

  module PVParticle : Hashgrid.PARTICLE with type t = PathVertex.t = PathVertex

  type cameraVertex = (* Bsdf.falsetype *) PathVertex.t
  type lightVertex = (* Bsdf.truetype *) PathVertex.t

  type cameraBSDF = (* Bsdf.falsetype *) Bsdf.t
  type lightBSDF = (* Bsdf.truetype *) Bsdf.t

  let make_cameraBSDF = Bsdf.make false
  let make_lightBSDF  = Bsdf.make true

  class type vertexcm =
            object
              method maxPathLength : int
              method minPathLength : int
              method mis : float -> float
              method misVcWeightFactor : float
              method ppm : bool
              method scene : SceneSphere.t Scene.t
            end

  module RangeQuery (* : Hashgrid.QUERY with module Particle = PathVertex *) = struct
    (* Range query used for PPM, BPT, and VCM. When HashGrid finds a vertex
     * within range -- Process() is called and vertex
     * merging is performed. BSDF of the camera vertex is used. *)
    module Particle = PVParticle

    type t = {
      mVertexCM        : vertexcm;
      mCameraPosition  : V.t;
      mCameraBsdf      : cameraBSDF;
      mCameraState     : subPathState;
      mutable mContrib : V.t;
    }

    let make aVertexCM aCameraPosition aCameraBsdf aCameraState = {
      mVertexCM       = aVertexCM;
      mCameraPosition = aCameraPosition;
      mCameraBsdf     = aCameraBsdf;
      mCameraState    = aCameraState;
      mContrib        = V.null ();
    }

    let getPosition self = self.mCameraPosition

    let getContrib self = self.mContrib

    let process self (aLightVertex : Particle.t) =
      let open PathVertex in
      let open Bsdf in
      (* Reject if full path length below/above min/max path length *)
      let mVertexCM = self.mVertexCM in
      let pathLength = aLightVertex.mPathLength + self.mCameraState.mPathLength in
      if pathLength > mVertexCM#maxPathLength || pathLength < mVertexCM#minPathLength
      then () (* self.mContrib *)
      else
        (* Retrieve light incoming direction in world coordinates *)
        let lightDirection = Bsdf.worldDirFix aLightVertex.mBsdf in
        (* float cosCamera, cameraBsdfDirPdfW, cameraBsdfRevPdfW; *)
        let cameraBsdfFactor = Bsdf.evaluate self.mCameraBsdf
                                             mVertexCM#scene
                                                         lightDirection in
        if isZero cameraBsdfFactor.oBSDF
        then () (* self.mContrib *)
        else

          let cameraBsdfDirPdfW = cameraBsdfFactor.oDirectPdfW *. (Bsdf.continuationProb self.mCameraBsdf)
          (* Even though this is pdf from camera BSDF, the continuation probability
           * must come from light BSDF, because that would govern it if light path
           * actually continued *)
          and cameraBsdfRevPdfW = cameraBsdfFactor.oReversePdfW *. (Bsdf.continuationProb aLightVertex.mBsdf) in

          (* Partial light sub-path MIS weight [tech. rep. (38)] *)
          let wLight = aLightVertex.dVCM *. mVertexCM#misVcWeightFactor +.
                         aLightVertex.dVM *. mVertexCM#mis cameraBsdfDirPdfW

          (* Partial eye sub-path MIS weight [tech. rep. (39)] *)
          and wCamera = self.mCameraState.dVCM *. mVertexCM#misVcWeightFactor +.
                          self.mCameraState.dVM *. mVertexCM#mis cameraBsdfRevPdfW in

          (* Full path MIS weight [tech. rep. (37)]. No MIS for PPM *)
          let misWeight = if mVertexCM#ppm then 1. else (1. /. (wLight +. 1. +. wCamera)) in

          self.mContrib <- V.add self.mContrib (V.scale (V.mul cameraBsdfFactor.oBSDF aLightVertex.mThroughput) misWeight)
  end

  module HG : Hashgrid.S with module Particle = PVParticle
                          and type Particle.t = PathVertex.t
                          and type query = RangeQuery.t
                                           = Hashgrid.Make(RangeQuery)

  class vertexCM (aScene : SceneSphere.t Scene.t) pathLengths aAlgorithm aRadiusFactor aRadiusAlpha aSeed =
  object(self)

    inherit Renderer.abstractRenderer aScene pathLengths as super

    val mutable mUseVM = false               (* Vertex merging (of some form) is used *)
    val mutable mUseVC = false               (* Vertex connection (BPT) is used *)
    val mutable mLightTraceOnly = false      (* Do only light tracing *)
    val mutable mPpm  = false                (* Do PPM, same terminates camera after first merge *)
    val mutable mRadiusAlpha = aRadiusAlpha  (* Radius reduction rate parameter *)
    val mutable mBaseRadius = 0.             (* Initial merging radius *)
    val mutable mMisVmWeightFactor = 0.      (* Weight of vertex merging (used in VC) *)
    val mutable mMisVcWeightFactor = 0.      (* Weight of vertex connection (used in VM) *)
    val mutable mScreenPixelCount  = 0.      (* Number of pixels *)
    val mutable mLightSubPathCount = 0.      (* Number of light sub-paths *)
    val mutable mVmNormalization   = 0.      (* 1 / (Pi * radius^2 * light_path_count) *)

    val mutable mLightVertices : lightVertex array = [| |]   (*!< Stored light vertices *)

    (* For light path belonging to pixel index [x] it stores
     * where it's light vertices end (begin is at [x-1]) *)
    val mutable mPathEnds : int Vector.t = Vector.make 1 0
    val mutable mHashGrid : Hashgrid.t option = None

    val         mRng = Rng.make aSeed

    method maxPathLength = mMaxPathLength
    method minPathLength = mMinPathLength
    method misVcWeightFactor = mMisVcWeightFactor
    method ppm = mPpm
    method scene = mScene

    initializer (
      let () =
        match aAlgorithm with
        | LightTrace ->
           mLightTraceOnly <- true
        | Ppm ->
           mPpm   <- true;
           mUseVM <- true
        | Bpm ->
           mUseVM <- true
        | Bpt ->
           mUseVC <- true
        | Vcm ->
           mUseVC <- true;
           mUseVM <- true
      in
      if mPpm
      then (
        (* We will check the scene to make sure it does not contain mixed
         * specular and non-specular materials *)
        let rec init i =
          if i < Scene.getMaterialCount mScene
          then
            let mat = Scene.getMaterial mScene i in
            let hasNonSpecular =
              (cmax @@ Material.diffuseReflectance mat) > 0.
              || (cmax @@ Material.phongReflectance mat) > 0.
            and hasSpecular =
              (cmax @@ Material.mirrorReflectance mat) > 0.
              || Material.ior mat > 0. in
            
            if hasNonSpecular && hasSpecular
            then (
              Printf.printf
                "*WARNING* Our PPM implementation cannot handle materials mixing
                 Specular and NonSpecular BSDFs. The extension would be
                 fairly straightforward. In SampleScattering for camera sub-paths
                 limit the considered events to Specular only.
                 Merging will use non-specular components, scattering will be specular.
                 If there is no specular component, the ray will terminate.

                 We are now switching from *PPM* to *BPM*, which can handle the scene.

                 ";
              mPpm <- false
            ) 
            else init (succ i)
        in init 0
      );
      mBaseRadius  <- aRadiusFactor *. SceneSphere.sceneRadius (Scene.sphere mScene);
      Printf.fprintf stderr "VertexCM : mUseVM = %d mUseVC = %d mLightTraceOnly = %d mPpm = %d mRadiusAlpha = %.03e mBaseRadius = %.03e\n"
                     ??mUseVM ??mUseVC ??mLightTraceOnly ??mPpm mRadiusAlpha mBaseRadius
   )

    (* Mis power, we use balance heuristic *)
    method mis aPdf =
      (* aPdf ** (*power*) *)
      aPdf

    (*////////////////////////////////////////////////////////////////////////
     * Camera tracing methods
     *////////////////////////////////////////////////////////////////////// *)
        
    (* Generates new camera sample given a pixel index *)
    method private generateCameraSample x y =
      let camera = Scene.camera mScene in

      (* Jitter pixel position *)
      let sample = V2f.add (V2f.of_tuple (float x, float y)) (Rng.getVec2f mRng) in

      Printf.fprintf stderr "sample = %a\n" pprintf_v2 sample;

      (* Generate ray *)
      let primaryRay = Camera.generateRay camera sample in

      (* Compute pdf conversion factor from area on image plane to solid angle on ray *)
      let cosAtCamera = V.dot (Camera.forward camera) (Ray.dir primaryRay) in
      let imagePointToCameraDist = (Camera.imagePlaneDist camera) /. cosAtCamera in
      let imageToSolidAngleFactor = (sqr imagePointToCameraDist) /. cosAtCamera in

      (* We put the virtual image plane at such a distance from the camera origin
       * that the pixel area is one and thus the image plane sampling pdf is 1.
       * The solid angle ray pdf is then equal to the conversion factor from
       * image plane area density to ray solid angle density *)
      let cameraPdfW = imageToSolidAngleFactor in
      let oCameraState = {
        mOrigin        = Ray.origin primaryRay;
        mDirection     = Ray.dir primaryRay;
        mThroughput    = V.make 1.0;
        mPathLength    = 1;
        mIsFiniteLight = false;
        mSpecularPath  = true;
        (* Eye sub-path MIS quantities. Implements [tech. rep. (31)-(33)] partially.
         * The evaluation is completed after tracing the camera ray in the eye sub-path loop. *)
        dVCM = self#mis (mLightSubPathCount /. cameraPdfW);
        dVC  = 0.;
        dVM  = 0.;
      } 
      in sample, oCameraState

    (* Returns the radiance of a light source when hit by a random ray,
     * multiplied by MIS weight. Can be used for both Background and Area lights.
     *
     * For Background lights:
     *    Has to be called BEFORE updating the MIS quantities.
     *    Value of aHitpoint is irrelevant (passing Vec3f(0))
     *
     * For Area lights:
     *    Has to be called AFTER updating the MIS quantities. *)
    method private getLightRadiance aLight aCameraState aHitpoint aRayDirection =
      (* We sample lights uniformly *)
      let lightCount    = Scene.getLightCount mScene in
      let lightPickProb = 1. /. (float lightCount) in

      let radiance, directPdfA, emissionPdfW = 
        Light.getRadianceEmission aLight (Scene.sphere mScene) aRayDirection aHitpoint in

      if isZero radiance
      then V.null ()
      else 
        (* If we see light source directly from camera, no weighting is required *)
        if aCameraState.mPathLength = 1
        then radiance
        else 
          (* When using only vertex merging, we want purely specular paths
           * to give radiance (cannot get it otherwise). Rest is handled
           * by merging and we should return 0. *)
          if mUseVM && not mUseVC
          then if aCameraState.mSpecularPath then radiance else V.null ()
          else
            let directPdfA  = directPdfA *. lightPickProb
            and emissionPdfW = emissionPdfW *. lightPickProb in

            (* Partial eye sub-path MIS weight [tech. rep. (43)].
             * If the last hit was specular, then dVCM == 0. *)
            let wCamera = (self#mis directPdfA) *. aCameraState.dVCM
                          +. (self#mis emissionPdfW) *. aCameraState.dVC in

            (* Partial light sub-path weight is 0 [tech. rep. (42)]. *)

            (* Full path MIS weight [tech. rep. (37)]. *)
            let misWeight = 1. /. (1. +. wCamera) in
            V.scale radiance misWeight

    (* Connects camera vertex to randomly chosen light point.
     * Returns emitted radiance multiplied by path MIS weight.
     * Has to be called AFTER updating the MIS quantities. *)
    method private directIllumination aCameraState aHitpoint aBsdf =
      let open Light in
      let open Bsdf in
      (* We sample lights uniformly *)
      let lightCount    = Scene.getLightCount mScene in
      let lightPickProb = 1. /. (float lightCount) in

      let lightID       = int_of_float ((Rng.getFloat mRng) *. (float lightCount)) in
      let rndPosSamples = Rng.getVec2f mRng in

      let light = Scene.getLightPtr mScene lightID in
      (*
        Vec3f directionToLight;
        float distance;
        float directPdfW, emissionPdfW, cosAtLight; *)
      let radiance = Light.illuminate light (Scene.sphere mScene) aHitpoint
                                      rndPosSamples true true in
      let directPdfW,emissionPdfW, cosAtLight =
        match radiance.oEmissionPdfW, radiance.oCosAtLight with
          Some e, Some c -> radiance.oDirectPdfW, e, c
        | _              -> assert false
      in

      (* If radiance == 0, other values are undefined, so have to early exit *)
      if isZero radiance.radiance
      then V.null ()
      else
        (*
        float bsdfDirPdfW, bsdfRevPdfW, cosToLight; *)
        let bsdfFactor = Bsdf.evaluate aBsdf mScene radiance.oDirectionToLight in
        if isZero bsdfFactor.oBSDF 
        then V.null ()
        else

          let continuationProbability = Bsdf.continuationProb aBsdf in
          
          (* If the light is delta light, we can never hit it
           * by BSDF sampling, so the probability of this path is 0 *)
          let bsdfDirPdfW = 
            if Light.isDelta light
            then 0.
            else bsdfFactor.oDirectPdfW *. continuationProbability in

          let bsdfRevPdfW = bsdfFactor.oReversePdfW *. continuationProbability in

          (* Partial light sub-path MIS weight [tech. rep. (44)].
           * Note that wLight is a ratio of area pdfs. But since both are on the
           * light source, their distance^2 and cosine terms cancel out.
           * Therefore we can write wLight as a ratio of solid angle pdfs,
           * both expressed w.r.t. the same shading point. *)
          let wLight = self#mis (bsdfDirPdfW /. (lightPickProb *. radiance.oDirectPdfW)) in

          (* Partial eye sub-path MIS weight [tech. rep. (45)].
           *
           * In front of the sum in the parenthesis we have Mis(ratio), where
           *    ratio = emissionPdfA / directPdfA,
           * with emissionPdfA being the product of the pdfs for choosing the
           * point on the light source and sampling the outgoing direction.
           * What we are given by the light source instead are emissionPdfW
           * and directPdfW. Converting to area pdfs and plugging into ratio:
           *    emissionPdfA = emissionPdfW * cosToLight / dist^2
           *    directPdfA   = directPdfW * cosAtLight / dist^2
           *    ratio = (emissionPdfW * cosToLight / dist^2) / (directPdfW * cosAtLight / dist^2)
           *    ratio = (emissionPdfW * cosToLight) / (directPdfW * cosAtLight)
           *
           * Also note that both emissionPdfW and directPdfW should be
           * multiplied by lightPickProb, so it cancels out. *)
          let wCamera = 
            (self#mis @@ 
               emissionPdfW *. bsdfFactor.oCosThetaGen
               /. (radiance.oDirectPdfW *. cosAtLight))
            *. (mMisVmWeightFactor +. aCameraState.dVCM
                +. aCameraState.dVC *. (self#mis bsdfRevPdfW)) in

          (* Full path MIS weight [tech. rep. (37)] *)
          let misWeight = 1. /. (wLight +. 1. +. wCamera) in
          let scale   = misWeight *. bsdfFactor.oCosThetaGen /. 
                          (lightPickProb *. radiance.oDirectPdfW) in
          let contrib = 
            V.scale (V.mul radiance.radiance bsdfFactor.oBSDF) scale in

          if isZero contrib
             || Scene.occluded mScene aHitpoint radiance.oDirectionToLight radiance.oDistance
          then V.null ()
          else contrib

    (* Connects an eye and a light vertex. Result multiplied by MIS weight, but
     * not multiplied by vertex throughputs. Has to be called AFTER updating MIS
     * constants. 'direction' is FROM eye TO light vertex. *)
    method private connectVertices (aLightVertex : PathVertex.t) aCameraBsdf aCameraHitpoint (aCameraState : subPathState) =
      let open PathVertex in
      let open Bsdf in
      (* Get the connection *)
      let direction   = V.sub aLightVertex.mHitpoint aCameraHitpoint in
      let dist2       = lenSqr direction in
      let distance    = sqrt dist2 in
      let direction   = V.scale direction (1. /. distance) in

      (* Evaluate BSDF at camera vertex *)
      (*
        float cosCamera, cameraBsdfDirPdfW, cameraBsdfRevPdfW; *)
      let cameraBsdfFactor = 
        Bsdf.evaluate aCameraBsdf mScene direction in

      if isZero cameraBsdfFactor.oBSDF
      then V.null ()
      else
        (* Camera continuation probability (for Russian roulette) *)
        let cameraCont = Bsdf.continuationProb aCameraBsdf in
        let cameraBsdfDirPdfW = cameraBsdfFactor.oDirectPdfW *. cameraCont
        and cameraBsdfRevPdfW = cameraBsdfFactor.oReversePdfW *. cameraCont in

        (* Evaluate BSDF at light vertex *)
        (*
          float cosLight, lightBsdfDirPdfW, lightBsdfRevPdfW; *)
        let lightBsdfFactor = 
          Bsdf.evaluate aLightVertex.mBsdf mScene (V.opp direction) in

        if isZero lightBsdfFactor.oBSDF
        then V.null ()
        else 
          (* Light continuation probability (for Russian roulette) *)
          let lightCont = Bsdf.continuationProb aLightVertex.mBsdf in
          let lightBsdfDirPdfW = lightBsdfFactor.oDirectPdfW *. lightCont
          and lightBsdfRevPdfW = lightBsdfFactor.oReversePdfW *. lightCont in

          (* Compute geometry term *)
          let geometryTerm = lightBsdfFactor.oCosThetaGen *. cameraBsdfFactor.oCosThetaGen /. dist2 in
          if geometryTerm < 0.
          then V.null ()
          else
            (* Convert pdfs to area pdf *)
            let cameraBsdfDirPdfA =
              pdfWtoA cameraBsdfDirPdfW distance lightBsdfFactor.oCosThetaGen
            and lightBsdfDirPdfA  =
              pdfWtoA lightBsdfDirPdfW distance cameraBsdfFactor.oCosThetaGen
            in
            (* Partial light sub-path MIS weight [tech. rep. (40)] *)
            let wLight = (self#mis cameraBsdfDirPdfA)
                         *. (mMisVmWeightFactor +. aLightVertex.dVCM 
                             +. aLightVertex.dVC *. (self#mis lightBsdfRevPdfW))

            (* Partial eye sub-path MIS weight [tech. rep. (41)] *)
            and wCamera = (self#mis lightBsdfDirPdfA) *. 
                            (mMisVmWeightFactor +. aCameraState.dVCM 
                             +. aCameraState.dVC *. (self#mis cameraBsdfRevPdfW)) in

            (* Full path MIS weight [tech. rep. (37)] *)
            let misWeight = 1. /. (wLight +. 1. +. wCamera) in

            let contrib = V.scale (V.mul cameraBsdfFactor.oBSDF lightBsdfFactor.oBSDF) 
                                  (misWeight *. geometryTerm) in

            if (isZero contrib) || (Scene.occluded mScene aCameraHitpoint direction distance)
            then V.null ()
            else contrib

    (*////////////////////////////////////////////////////////////////////////
     * Light tracing methods
     *////////////////////////////////////////////////////////////////////// *)

    (* Samples light emission *)
    method private generateLightSample () =
      let open Light in
      
      (* We sample lights uniformly *)
      let lightCount    = Scene.getLightCount mScene in
      let lightPickProb = 1. /. (float lightCount) in

      let lightID       = int_of_float @@ (Rng.getFloat mRng) *. (float lightCount)
      and rndDirSamples = Rng.getVec2f mRng
      and rndPosSamples = Rng.getVec2f mRng in
      (* Printf.fprintf stderr "lightID = %d  rndDir = %a  rndPos = %a\n" lightID pprintf_v2 rndDirSamples pprintf_v2 rndPosSamples;*)
      let light = Scene.getLightPtr mScene lightID in
      
      (* float emissionPdfW, directPdfW, cosLight; *)
      let emit = 
        Light. emit light (Scene.sphere mScene) rndDirSamples rndPosSamples
                   true true in
      let mOrigin      = emit.oPosition
      and mDirection   = emit.oDirection
      and emissionPdfW = emit.oEmissionPdfW *. lightPickProb
      and directPdfW   = match emit.oDirectPdfA with
          Some f -> f *. lightPickProb 
        | None -> assert false in

      let mThroughput    = V.scale emit.energy (1. /. emissionPdfW)
      and mPathLength    = 1
      and mIsFiniteLight = Light.isFinite light in

      (* Light sub-path MIS quantities. Implements [tech. rep. (31)-(33)] partially.
       * The evaluation is completed after tracing the emission ray in the light sub-path loop.
       * Delta lights are handled as well [tech. rep. (48)-(50)]. *)
      let dVCM = self#mis (directPdfW /. emissionPdfW) in
      let dVC =
        if not @@ Light.isDelta light
        then let usedCosLight =
               match emit.oCosThetaLight, mIsFiniteLight with
               | Some f, true  -> f 
               | Some _, false -> 1.
               | _ -> assert false in
             self#mis (usedCosLight /. emissionPdfW)
        else 0. in
      let dVM = dVC *. mMisVcWeightFactor in {
        mOrigin;
        mDirection;
        mThroughput;
        mPathLength;
        mIsFiniteLight;
        mSpecularPath = false;
        dVCM;
        dVC;
        dVM
      }

    (* Computes contribution of light sample to camera by splatting is onto the
     * framebuffer. Multiplies by throughput (obviously, as nothing is returned). *)
    method private connectToCamera aLightState aHitpoint aBsdf =
      let open Bsdf in
      prerr_endline "connectToCamera";
      let camera    = Scene.camera mScene in
      let directionToCamera = V.sub (Camera.position camera) aHitpoint in

      (* Check point is in front of camera *)
      let cosAtCamera = -.(V.dot (Camera.forward camera) directionToCamera) in
      if cosAtCamera <= 0.
      then ()
      else
        (* Check it projects to the screen (and where) *)
        let imagePos = Camera.worldToRaster camera aHitpoint in
        if not (Camera.checkRaster camera imagePos)
        then ()
        else 
          (* Compute distance and normalize direction to camera *)
          let distEye2 = lenSqr directionToCamera in
          let distance = sqrt distEye2 in
          let directionToCamera = V.scale directionToCamera (1./.distance) in

          (* Get the BSDF *)
          (* float cosToCamera, bsdfDirPdfW, bsdfRevPdfW; *)
          let bsdfFactor = Bsdf.evaluate aBsdf mScene directionToCamera in

          if isZero bsdfFactor.oBSDF
          then ()
          else 
            let bsdfRevPdfW = bsdfFactor.oReversePdfW *. (Bsdf.continuationProb aBsdf) in
            let cosAtCamera = -.(V.dot (Camera.forward camera) directionToCamera) in
            (* Compute pdf conversion factor from image plane area to surface area *)
            let imagePointToCameraDist  = (Camera.imagePlaneDist camera) /. cosAtCamera in
            let imageToSolidAngleFactor = (sqr imagePointToCameraDist) /. cosAtCamera in
            let imageToSurfaceFactor    = 
              imageToSolidAngleFactor *. (abs_float bsdfFactor.oCosThetaGen) /. (sqr distance) in

            (* We put the virtual image plane at such a distance from the camera origin
             * that the pixel area is one and thus the image plane sampling pdf is 1.
             * The area pdf of aHitpoint as sampled from the camera is then equal to
             * the conversion factor from image plane area density to surface area density *)
            let cameraPdfA = imageToSurfaceFactor in

            (* Partial light sub-path weight [tech. rep. (46)]. Note the division by
             * mLightPathCount, which is the number of samples this technique uses.
             * This division also appears a few lines below in the framebuffer accumulation. *)
            let wLight = (self#mis @@ cameraPdfA /. mLightSubPathCount)
                         *. (mMisVmWeightFactor +. aLightState.dVCM
                             +. aLightState.dVC *. (self#mis bsdfRevPdfW)) in

            (* Partial eye sub-path weight is 0 [tech. rep. (47)] *)

            (* Full path MIS weight [tech. rep. (37)]. No MIS for traditional light tracing. *)
            let misWeight = if mLightTraceOnly then 1. else (1. /. (wLight +. 1.)) in

            let surfaceToImageFactor = 1. /. imageToSurfaceFactor in

            (* We divide the contribution by surfaceToImageFactor to convert the (already
             * divided) pdf from surface area to image plane area, w.r.t. which the
             * pixel integral is actually defined. We also divide by the number of samples
             * this technique makes, which is equal to the number of light sub-paths *)
            let contrib = V.scale (V.scale (V.mul aLightState.mThroughput 
                                         bsdfFactor.oBSDF)
                                           misWeight)
                                  (1. /. (mLightSubPathCount *. surfaceToImageFactor)) in

            if not @@ isZero contrib
            then if Scene.occluded mScene aHitpoint directionToCamera distance 
                 then ()
                 else let () = Printf.fprintf stderr "adding camera color : %a\n" pprintf_v contrib in 
                      Framebuffer.addColor mFramebuffer imagePos contrib
            else ()

    (* Samples a scattering direction camera/light sample according to BSDF. *)
    method private sampleScattering aBsdf aHitPoint aoState =
      let open Bsdf in
      Printf.fprintf stderr "sampleScattering %a %a %a\n" Bsdf.dump aBsdf pprintf_v aHitPoint dumpPathState aoState;
      (* x,y for direction, z for component. No rescaling happens *)
      let rndTriplet  = Rng.getVec3f mRng in
      (*
        float bsdfDirPdfW, cosThetaOut;
        uint  sampledEvent;
       *)
      let bsdfFactor = Bsdf. sample aBsdf mScene rndTriplet in
      
      if isZero bsdfFactor.oBSDF
      then None
      else 
        (* If we sampled specular event, then the reverse probability
         * cannot be evaluated, but we know it is exactly the same as
         * forward probability, so just set it. If non-specular event happened,
         * we evaluate the pdf *)
        let bsdfDirPdfW = bsdfFactor.oPdfW in
        let bsdfRevPdfW = 
          if bsdfFactor.oSampledEvent <> Reflect && bsdfFactor.oSampledEvent <> Refract
          then Bsdf.pdf aBsdf ~aEvalRevPdf:true mScene bsdfFactor.oWorldDirGen
          else bsdfDirPdfW in
        Printf.fprintf stderr "bsdfDirPdfW = %.03e bsdfRevPdfW = %.03e\n" bsdfDirPdfW bsdfRevPdfW;
        (* Russian roulette *)
        let contProb = Bsdf.continuationProb aBsdf in
        if Rng.getFloat mRng > contProb 
        then None
        else
          let bsdfDirPdfW = bsdfDirPdfW *. contProb
          and bsdfRevPdfW = bsdfRevPdfW *. contProb
          and cosThetaOut = bsdfFactor.oCosThetaGen in
          (* Sub-path MIS quantities for the next vertex. Only partial - the
           * evaluation is completed when the actual hit point is known,
           * i.e. after tracing the ray, in the sub-path loop. *)
          let mOrigin     = aHitPoint
          and mThroughput = V.mul aoState.mThroughput
                                  (V.scale bsdfFactor.oBSDF (cosThetaOut /. bsdfDirPdfW)) in
          if bsdfFactor.oSampledEvent = Reflect || bsdfFactor.oSampledEvent = Refract 
          then (
            (* Specular scattering case [tech. rep. (53)-(55)] (partially, as noted above) *)
            assert (bsdfDirPdfW = bsdfRevPdfW);
            let misv = self#mis cosThetaOut in
            let dVCM = 0.
            (*aoState.dVC *= Mis(cosThetaOut / bsdfDirPdfW) * Mis(bsdfRevPdfW); 
             *aoState.dVM *= Mis(cosThetaOut / bsdfDirPdfW) * Mis(bsdfRevPdfW); *)
            and dVC = aoState.dVC *. misv
            and dVM = aoState.dVM *. misv
            and mSpecularPath = aoState.mSpecularPath in
            Some { aoState with
                   mOrigin;
                   mDirection = bsdfFactor.oWorldDirGen;
                   mThroughput;
                   dVCM;
                   dVC;
                   dVM;
                   mSpecularPath;
                 }
          ) else
            (* Implements [tech. rep. (34)-(36)] (partially, as noted above) *)
            let dVC = (self#mis @@ cosThetaOut /. bsdfDirPdfW) *. 
                        (aoState.dVC *. (self#mis bsdfRevPdfW) +.
                           aoState.dVCM +. mMisVmWeightFactor)

            and dVM = (self#mis @@ cosThetaOut /. bsdfDirPdfW) *.
                        (aoState.dVM *. (self#mis bsdfRevPdfW) +.
                           aoState.dVCM *. mMisVcWeightFactor +. 1.)
            and dVCM = self#mis @@ 1. /. bsdfDirPdfW
            and mSpecularPath = false in
            Some { aoState with
                   mOrigin;
                   mDirection = bsdfFactor.oWorldDirGen;
                   mThroughput;
                   dVCM;
                   dVC;
                   dVM;
                   mSpecularPath;
                 }

    (*////////////////////////////////////////////////////////////////////////
     * Generate light paths
     *////////////////////////////////////////////////////////////////////// *)
    method private generateLightPaths pathCount =
      prerr_endline "generateLightPaths"; 
      (*////////////////////////////////////////////////////////////////////////
       * Trace light path *)
      let rec traceLightPath lightVertices lightVertices_size lightState pathLength =
        Printf.fprintf stderr "lightState : %a\n" dumpPathState lightState;
        let ray = Ray.make lightState.mOrigin lightState.mDirection 0.
        and isect = Isect.default in
        Printf.fprintf stderr "ray : %a\n" Ray.dump ray;
        match Scene.intersect mScene ray isect with
          None       -> (prerr_endline "no intersect"; lightVertices, lightVertices_size)
        | Some isect ->
           Printf.fprintf stderr "isect : %a\n" Isect.dump isect;
           let hitPoint = V.add (Ray.origin ray) (V.scale (Ray.dir ray) (Isect.dist isect)) in
           let isect = Isect.set_dist isect ((Isect.dist isect) +. eps_ray) in
           let bsdf = make_lightBSDF ray isect mScene in
           if not @@ Bsdf.isValid bsdf 
           then
             let () = prerr_endline "notValid bsdf" in
             lightVertices, lightVertices_size 
           else
             (* Update the MIS quantities before storing them at the vertex.
              * These updates follow the initialization in GenerateLightSample() or
              * SampleScattering(), and together implement equations [tech. rep. (31)-(33)]
              * or [tech. rep. (34)-(36)], respectively. *)
             
             (* Infinite lights use MIS handled via solid angle integration,
              * so do not divide by the distance for such lights [tech. rep. Section 5.1] *)
             let dVCM =
               if lightState.mPathLength > 1 || lightState.mIsFiniteLight 
               then lightState.dVCM *. (self#mis @@ sqr (Isect.dist isect))
               else lightState.dVCM in
             let mcos = self#mis @@ abs_float (Bsdf.cosThetaFix bsdf) in
             let lightState = {
               lightState with
               mPathLength = pathLength;
               dVCM = dVCM /. mcos;
               dVC  = lightState.dVC /. mcos;
               dVM  = lightState.dVM /. mcos;
             } in
             let lightVertices, lightVertices_size  = 
               (* Store vertex, unless BSDF is purely specular, which prevents
                * vertex connections and merging *)
               if (not @@ Bsdf.isDelta bsdf) && (mUseVC || mUseVM)
               then
                 let () = Printf.fprintf stderr "!(Bsdf.isDelta bsdf {%d}) && (mUseVC {%d} || mUseVM {%d})\n"
                                         ??(Bsdf.isDelta bsdf) ??mUseVC ??mUseVM in
                 let lightVertex = 
                   let open PathVertex in {
                     tFromLight  = true;
                     mHitpoint   = hitPoint;
                     mThroughput = lightState.mThroughput;
                     mPathLength = lightState.mPathLength;
                     mBsdf       = bsdf;
                     dVCM = lightState.dVCM;
                     dVC  = lightState.dVC;
                     dVM  = lightState.dVM;
                   } in lightVertex::lightVertices, succ lightVertices_size 
               else 
                 let () = Printf.fprintf stderr "NOT (!(Bsdf.isDelta bsdf {%d}) && (mUseVC {%d} || mUseVM {%d}))\n"
                                         ??(Bsdf.isDelta bsdf) ??mUseVC ??mUseVM in
                 lightVertices, lightVertices_size 
             in
             (* Connect to camera, unless BSDF is purely specular *)
             if (not @@ Bsdf.isDelta bsdf) && (mUseVC || mLightTraceOnly)
                && (lightState.mPathLength + 1 >= mMinPathLength)
             then self#connectToCamera lightState hitPoint bsdf;
             (* Terminate if the path would become too long after scattering *)
             if lightState.mPathLength + 2 > mMaxPathLength
             then lightVertices, lightVertices_size 
             else 
               (* Continue random walk *)
               match self#sampleScattering bsdf hitPoint lightState with
                 None -> lightVertices, lightVertices_size
               | Some lightState -> traceLightPath lightVertices lightVertices_size lightState (succ pathLength)
      in 
      let rec loop pathIdx lightVertices lightVertices_size =
        let () = Printf.fprintf stderr "generateLightPaths loop : pathIdx = %d\n" pathIdx in
        let lightState = self#generateLightSample () in
        let lightVertices, lightVertices_size = 
          traceLightPath lightVertices lightVertices_size lightState 1 in
        Printf.fprintf stderr "traceLightPath -> size = %d\n" lightVertices_size;
        Vector.set mPathEnds pathIdx lightVertices_size;
        if pathIdx >= pred pathCount
        then lightVertices
        else loop (succ pathIdx) lightVertices lightVertices_size
      in
      let lightVertices = loop 0 [] 0 in
      mLightVertices <- Array.of_list (List.rev lightVertices)

    (* --------------------------------------------------------- *)

    method runIteration aIteration =

      (* While we have the same number of pixels (camera paths)
       * and light paths, we do keep them separate for clarity reasons *)
      let resolution = Camera.resolution @@ Scene.camera mScene in
      let resX, resY = V2i.to_tuple resolution in
      let pathCount  = resX * resY in
      mScreenPixelCount  <- float @@ resX * resY;
      mLightSubPathCount <- float @@ resX * resY;

      (* Setup our radius, 1st iteration has aIteration == 0, thus offset *)
      let radius = mBaseRadius /. ((float @@ aIteration + 1) ** (0.5 *. (1. -. mRadiusAlpha))) in
      (* Purely for numeric stability *)
      let radius = max radius 1e-7 in
      let radiusSqr = sqr radius in
      Printf.fprintf stderr "VertexCM.runIteration mBaseRadius = %.03e mRadiusAlpha = %.03e radius = %.03e\n" mBaseRadius mRadiusAlpha radius;
      (* Factor used to normalise vertex merging contribution.
       * We divide the summed up energy by disk radius and number of light paths *)
      mVmNormalization <- 1. /. (radiusSqr *. pi *. mLightSubPathCount);

      (* MIS weight constant [tech. rep. (20)], with n_VC = 1 and n_VM = mLightPathCount *)
      let etaVCM = (pi *. radiusSqr) *. mLightSubPathCount in
      mMisVmWeightFactor <- if mUseVM then self#mis etaVCM else 0.;
      mMisVcWeightFactor <- if mUseVC then self#mis (1. /. etaVCM) else 0.;

      (* Clear path ends, nothing ends anywhere *)
      Vector.resize mPathEnds pathCount;
      
      (* Remove all light vertices and reserve space for some *)
      self#generateLightPaths pathCount;

      (*////////////////////////////////////////////////////////////////////////
       * Build hash grid
       *////////////////////////////////////////////////////////////////////// *)

      (* Only build grid when merging (VCM, BPM, and PPM) *)
      if mUseVM
      then
        (* The number of cells is somewhat arbitrary, but seems to work ok *)
        (* mHashGrid.Reserve(pathCount); *)
        mHashGrid <- Some (HG.build mLightVertices pathCount radius);


      (*////////////////////////////////////////////////////////////////////////
       * Generate camera paths
       *////////////////////////////////////////////////////////////////////// *)
      
      (*//////////////////////////////////////////////////////////////////////
       * Trace camera path *)
      let rec trace pathIdx color cameraState =
        let () = Printf.fprintf stderr "cameraState = %a\n" dumpPathState cameraState in
        let ray = Ray.make cameraState.mOrigin cameraState.mDirection 0.
        and isect = Isect.default in
        let () = Printf.fprintf stderr "pathLength = %d - ray = %a\n" cameraState.mPathLength Ray.dump ray in
        (* Get radiance from environment *)
        match Scene.intersect mScene ray isect with
          None -> (
          let () = prerr_endline "no camera intersect" in
          match Scene.getBackground mScene with
            Some background -> (
            if cameraState.mPathLength >= mMinPathLength
            then V.add color (
                         V.mul cameraState.mThroughput 
                               (self#getLightRadiance (Light.BackgroundLight background) cameraState
                                                      (V.null ()) (Ray.dir ray)))
            else color
          )
          | None -> color
        )
        | Some isect ->
           let () = Printf.fprintf stderr "camera intersect %a\n" Isect.dump isect in
           let hitPoint = V.add (Ray.origin ray) (V.scale (Ray.dir ray) (Isect.dist isect)) in
           Printf.fprintf stderr "hitpoint = %a\n" pprintf_v hitPoint;

           let isect = Isect.set_dist isect ((Isect.dist isect) +. eps_ray) in
           let bsdf = make_cameraBSDF ray isect mScene in
           if not @@ Bsdf.isValid bsdf
           then let () = prerr_endline "notValid CameraBSDF" in color
           else
             let () = Printf.fprintf stderr "isValid CameraBSDF %a\n" Bsdf.dump bsdf in
             (* Update the MIS quantities, following the initialization in
              * GenerateLightSample() or SampleScattering(). Implement equations
              * [tech. rep. (31)-(33)] or [tech. rep. (34)-(36)], respectively. *)
             let dVCM = cameraState.dVCM *. (self#mis (sqr (Isect.dist isect)))
             and mcos = self#mis (abs_float @@ Bsdf.cosThetaFix bsdf) in
             let cameraState = {
               cameraState with
               dVCM = dVCM /. mcos;
               dVC  = cameraState.dVC /. mcos;
               dVM  = cameraState.dVM /. mcos
             } in 
             Printf.fprintf stderr "ncameraState = %a\n" dumpPathState cameraState;
             (* Light source has been hit; terminate afterwards, since
              * our light sources do not have reflective properties *)
             if (Isect.lightID isect) >= 0
             then (
               let light = Scene.getLightPtr mScene (Isect.lightID isect) in
               if cameraState.mPathLength >= mMinPathLength
               then
                 V.add color(V.mul cameraState.mThroughput
                                   (self#getLightRadiance light cameraState hitPoint (Ray.dir ray)))
               else color
             ) else
               (* Terminate if eye sub-path is too long for connections or merging *)
               if cameraState.mPathLength >= mMaxPathLength
               then color
               else
                 (*//////////////////////////////////////////////////////////////
                  * Vertex connection: Connect to a light source *) 
                 let color =
                   if not @@ Bsdf.isDelta bsdf && mUseVC
                      && (succ cameraState.mPathLength) >= mMinPathLength
                   then
                     V.add color (V.mul cameraState.mThroughput
                                        (self#directIllumination cameraState hitPoint bsdf))
                   else color
                 in
                 (*//////////////////////////////////////////////////////////////
                  * Vertex connection: Connect to light vertices *)
                 let color =
                   if not @@ Bsdf.isDelta bsdf && mUseVC
                   then
                     let () = prerr_endline "!bsdf.IsDelta() && mUseVC" in
                     (* For VC, each light sub-path is assigned to a particular eye
                      * sub-path, as in traditional BPT. It is also possible to
                      * connect to vertices from any light path, but MIS should
                      * be revisited. *)
                     let x = if pathIdx = 0 then 0 else (Vector.get mPathEnds (pathIdx-1))
                     and y = Vector.get mPathEnds pathIdx in
                     let rec subpath color i : V.t =
                       let open PathVertex in
                       if i = y 
                       then color
                       else
                         let lightVertex = mLightVertices.(i) in
                         
                         if lightVertex.mPathLength + 1 + cameraState.mPathLength < mMinPathLength
                         then subpath color (succ i)
                         else
                           (* Light vertices are stored in increasing path length
                            * order; once we go above the max path length, we can
                            * skip the rest *)
                           if lightVertex.mPathLength + 1 + cameraState.mPathLength > mMaxPathLength
                           then color
                           else
                             let color = 
                               V.add color
                                     (V.mul (V.mul cameraState.mThroughput lightVertex.mThroughput)
                                            (self#connectVertices lightVertex bsdf hitPoint cameraState))
                             in subpath color (succ i)
                     in
                     subpath color x
                   else color
                 in
                 (*//////////////////////////////////////////////////////////////
                  * Vertex merging: Merge with light vertices *)
                 let merging color =
                   prerr_endline "vertex merging";
                   match mHashGrid with
                     None -> assert false
                   | Some mHashGrid ->
                      let query = RangeQuery.make (self :> vertexcm) hitPoint bsdf cameraState in
                      HG.process mHashGrid mLightVertices query;
                      V.add color
                            (V.scale (V.mul cameraState.mThroughput (RangeQuery.getContrib query))
                                     mVmNormalization)
                            
                 in
                 let cont color =
                   match self#sampleScattering bsdf hitPoint cameraState with
                     None -> color
                   | Some cameraState -> trace pathIdx color {cameraState with mPathLength = cameraState.mPathLength + 1}
                 in
                 match not @@ Bsdf.isDelta bsdf && mUseVM, mPpm with
                   true, true -> merging color
                 | true, false -> cont @@ merging color
                 | _ -> cont color
      in
      (* Unless rendering with traditional light tracing *)
      let rec render x y =
        if x < resX && y < resY && not mLightTraceOnly
        then
          let pathIdx = y * resX + x in
          Printf.fprintf stderr "Camera pathIdx = %d\n" pathIdx;
          let screenSample, cameraState = self#generateCameraSample x y in
          let color = trace pathIdx (V.null ()) {cameraState with mPathLength = 1} in
          let () = dump_v "adding color" color in
          Framebuffer. addColor mFramebuffer (v2i_of_v2f screenSample) color;
          if x >= pred resX
          then render 0 (succ y)
          else render (succ x) y
      in
      render 0 0;
      mIterations <- succ mIterations
                          
  end

  let renderer aScene pathLengths aAlgorithm aRadiusFactor aRadiusAlpha aSeed =
    (new vertexCM aScene pathLengths aAlgorithm aRadiusFactor aRadiusAlpha aSeed :> Renderer.abstractRenderer)

end
