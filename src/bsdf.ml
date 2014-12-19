(*
 * Copyright (C) 2012, Tomas Davidovic (http://www.davidovic.cz)
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * (The above is MIT License: http://en.wikipedia.org/wiki/MIT_License)
 *)

open Utils

(*////////////////////////////////////////////////////////////////////////
// BSDF, most magic happens here
//
// One of important conventions is prefixing direction with World when
// are in world coordinates and with Local when they are in local frame,
// i.e., mFrame.
//
// Another important convention if suffix Fix and Gen.
// For PDF computation, we need to know which direction is given (Fix),
// and which is the generated (Gen) direction. This is important even
// when simply evaluating BSDF.
// In BPT, we call Evaluate() when directly connecting to light/camera.
// This gives us both directions required for evaluating BSDF.
// However, for MIS we also need to know probabilities of having sampled
// this path via BSDF sampling, and we need that for both possible directions.
// The Fix/Gen convention (along with Direct and Reverse for PDF) clearly
// establishes which PDF is which.
//
// The BSDF is also templated by direction of tracing, whether from camera
// (BSDF<false>) or from light (BSDF<true>). This is identical to Veach's
// Adjoint BSDF (except the name is more straightforward).
// For us this is only used when refracting. *)

let eps_phong = 1e-3

type truetype
type falsetype

type componentProbabilities = {
  diffProb  : float;
  phongProb : float;
  reflProb  : float;
  refrProb  : float;
}

let nullProbabilities = {
  diffProb  = 0.;
  phongProb = 0.;
  reflProb  = 0.;
  refrProb  = 0.;
}

let dumpProbs out p =
  Printf.fprintf out "{%.03e; %.03e; %.03e; %.03e}" p.diffProb p.phongProb p.reflProb p.refrProb


type events = 
  | NONE
  | Diffuse
  | Phong
  | Reflect
  | Refract
(*
  | Specular    (* = (Reflect  | Refract) *)
  | NonSpecular (* = (Diffuse  | Phong) *)
  | All         (* = (Specular | NonSpecular *)
 *)

(* 'a must be either truetype or falsetype and should correspond to mFixIsLight *)
type (*'a *) t = {
   mMaterialID       : int;                    (*!< Id of scene material, < 0 ~ invalid *)
   mFrame            : Frame.t;                (*!< Local frame of reference *)
   mLocalDirFix      : V.t;                    (*!< Incoming (fixed) direction, in local *)
   mIsDelta          : bool;                   (*!< True when material is purely specular *)
   mProbabilities    : componentProbabilities; (*!< Sampling probabilities *)
   mContinuationProb : float;                  (*!< Russian roulette probability *)
   mReflectCoeff     : float;                  (*!< Fresnel reflection coefficient (for glass) *)
   mFixIsLight       : bool;
}

let dump out t =
  Printf.fprintf out "{ matID = %d; mFrame = %a; mLocalDirFix = %a; mIsDelta = %d; mProbabilities = %a; mContinuationProb = %.03e; mReflectCoeff = %.03e }" t.mMaterialID Frame.dump t.mFrame pprintf_v t.mLocalDirFix ??(t.mIsDelta) dumpProbs t.mProbabilities t.mContinuationProb t.mReflectCoeff

let invalid () = {
  mMaterialID       = -1;
  mFrame            = Frame.identity ();
  mLocalDirFix      = V.null ();
  mIsDelta          = false;
  mProbabilities    = nullProbabilities;
  mContinuationProb = 0.0;
  mReflectCoeff     = 0.0;
  mFixIsLight       = false;
}

let isValid self = self.mMaterialID >= 0
let isDelta self = self.mIsDelta
let continuationProb self = self.mContinuationProb
let cosThetaFix self = V.get self.mLocalDirFix 2
let worldDirFix self = Frame.toWorld self.mFrame self.mLocalDirFix

(*//////////////////////////////////////////////////////////////////////////
 * Pdf methods
 *////////////////////////////////////////////////////////////////////////*)

(* ok *)
let pdfDiffuse self aMaterial aLocalDirGen oDirectPdfW oReversePdfW =
  if self.mProbabilities.diffProb = 0.
  then oDirectPdfW, oReversePdfW
  else 
    let computeDiffuse pdfW z =
      pdfW +. self.mProbabilities.diffProb *. (max 0. @@ z *. inv_pi) in
    let oDirectPdfW = computeDiffuse oDirectPdfW (V.get aLocalDirGen 2)
    and oReversePdfW = computeDiffuse oReversePdfW (V.get self.mLocalDirFix 2) in
    oDirectPdfW, oReversePdfW

(* ok *)
let pdfPhong self aMaterial aLocalDirGen oDirectPdfW oReversePdfW =
  if self.mProbabilities.phongProb = 0.
  then oDirectPdfW, oReversePdfW
  else 
    (* assumes this is never called when rejectShadingCos(oLocalDirGen.z) is true *)
    let reflLocalDirIn = reflectLocal self.mLocalDirFix in
    let dot_R_Wi = V.dot reflLocalDirIn aLocalDirGen in
    if dot_R_Wi <= eps_phong
    then oDirectPdfW, oReversePdfW
    else 
      (* the sampling is symmetric *)
      let exponent = Material.phongExponent aMaterial in
      let pdfW = powerCosHemispherePdfW reflLocalDirIn aLocalDirGen exponent in
      let pdfW = pdfW *. self.mProbabilities.phongProb in
      oDirectPdfW +. pdfW, oReversePdfW +. pdfW

(* ok *)
(* \brief Given a direction, evaluates Pdf
 *
 * By default returns PDF with which would be aWorldDirGen
 * generated from mLocalDirFix. When aEvalRevPdf == true,
 * it provides PDF for the reverse direction.
 *)
let pdf self ?(aEvalRevPdf=false) aScene aWorldDirGen =
  let localDirGen = Frame.toLocal self.mFrame aWorldDirGen in
  if (V.get localDirGen 2) *. (V.get self.mLocalDirFix 2) < 0.
  then 0.0
  else 
    let mat = Scene.getMaterial aScene self.mMaterialID in
    let directPdfW, reversePdfW = pdfDiffuse self mat localDirGen 0. 0. in
    let directPdfW, reversePdfW = pdfPhong self mat localDirGen directPdfW reversePdfW in
    if aEvalRevPdf then reversePdfW else directPdfW

(*//////////////////////////////////////////////////////////////////////////
 * Sampling methods
 * All sampling methods take material, 2 random numbers [0-1[,
 * and return BSDF factor, generated direction in local coordinates, and PDF
 */////////////////////////////////////////////////////////////////////////*)

type samplingMethod_result = {
  oBSDF         : V.t;
  oLocalDirGen  : V.t;
  oPdfW         : float;
}

let null_sampling = {
  oBSDF        = V.null ();
  oLocalDirGen = V.null ();
  oPdfW        = 0.;
}

(* ok *)
let sampleDiffuse self aMaterial aRndTuple pdfW =
  if (V.get self.mLocalDirFix 2) < eps_cosine
  then {null_sampling with oPdfW = pdfW}
  else 
    let unweightedPdfW, oLocalDirGen = sampleCosHemisphereW aRndTuple in
    let oPdfW = pdfW +. unweightedPdfW *. self.mProbabilities.diffProb in
    {
      oBSDF = V.scale (Material.diffuseReflectance aMaterial) inv_pi;
      oLocalDirGen;
      oPdfW;
    }

(* ok *)
let samplePhong self aMaterial aRndTuple pdfW =
  let _, oLocalDirGen = 
    samplePowerCosHemisphereW aRndTuple (Material.phongExponent aMaterial) in
  (* Due to numeric issues in MIS, we actually need to compute all pdfs
     exactly the same way all the time!!! *)
  let reflLocalDirFixed = reflectLocal self.mLocalDirFix in
  let oLocalDirGen = 
    let frame = Frame.setFromZ reflLocalDirFixed in
    Frame.toWorld frame oLocalDirGen in

  let dot_R_Wi = V.dot reflLocalDirFixed oLocalDirGen in
  if dot_R_Wi <= eps_phong 
  then {null_sampling with oPdfW = pdfW; oLocalDirGen}
  else
    let exponent = Material.phongExponent aMaterial in
    let oPdfW, _ = pdfPhong self aMaterial oLocalDirGen pdfW pdfW in
    let rho = V.scale (Material.phongReflectance aMaterial)
                      @@ (exponent +. 2.) *. 0.5 *. inv_pi in
    {
      oBSDF = V.scale rho (dot_R_Wi ** exponent);
      oLocalDirGen;
      oPdfW;
    }

(* ok *)
let sampleReflect self aMaterial aRndTuple pdfW =
  let oLocalDirGen = reflectLocal self.mLocalDirFix in
  let oPdfW = pdfW +. self.mProbabilities.reflProb in
  (* BSDF is multiplied (outside) by cosine (oLocalDirGen.z),
     for mirror this shouldn't be done, so we pre-divide here instead *)
  {
    oBSDF =  V.scale (Material.mirrorReflectance aMaterial) (self.mReflectCoeff /.
              abs_float (V.get oLocalDirGen 2));
    oLocalDirGen;
    oPdfW;
  }

(* ok *)
let sampleRefract self aMaterial aRndTuple pdfW =
  let ior = Material.ior aMaterial in
  if ior < 0.
  then { null_sampling with oPdfW = pdfW }
  else
    let localDirFixX, localDirFixY, cosI = tuple_of_v3 self.mLocalDirFix in
    let etaIncOverEtaTrans, cosI, cosT =
      if cosI < 0. (* hit from inside *)
      then ior, -.cosI, 1.
      else 1. /. ior, cosI, -1.
    in
    
    let sinI2 = 1. -. (sqr cosI) in
    let sinT2 = (sqr etaIncOverEtaTrans) *. sinI2 in
    if sinT2 < 1. (* no total internal reflection *)
    then
      let cosT = cosT *. sqrt (max 0. @@ 1. -. sinT2) in
      let oLocalDirGen = 
        v3_of_tuple (
            -.etaIncOverEtaTrans *. localDirFixX,
            -.etaIncOverEtaTrans *. localDirFixY,
            cosT) in
      let oPdfW = pdfW +. self.mProbabilities.refrProb in
      let refractCoeff = 1. -. self.mReflectCoeff in
      (* only camera paths are multiplied by this factor, and etas
             are swapped because radiance flows in the opposite direction *)
      let oBSDF =
        if not self.mFixIsLight 
        then V.make (refractCoeff *. (sqr etaIncOverEtaTrans) /. (abs_float cosT))
        else V.make (refractCoeff /. (abs_float cosT))
      in {
        oBSDF;
        oLocalDirGen;
        oPdfW;    
      }
    (* else total internal reflection, do nothing *)
    else { null_sampling with oPdfW = pdfW }


(*//////////////////////////////////////////////////////////////////////////
 * Evaluation methods
 *////////////////////////////////////////////////////////////////////////*)

type evaluating = {
  oDirectPdfW  : float;
  oReversePdfW : float;
  oBSDF        : V.t;
}

let null_evaluating = {
  oDirectPdfW  = 0.;
  oReversePdfW = 0.;
  oBSDF        = V.null ();
}

type evaluate_result = {
  oDirectPdfW  : float;
  oReversePdfW : float;
  oBSDF        : V.t;
  oLocalDirGen : V.t;
  oCosThetaGen : float;
}

let null_eval oLocalDirGen = {
  oDirectPdfW  = 0.;
  oReversePdfW = 0.;
  oBSDF        = V.null ();
  oLocalDirGen;
  oCosThetaGen = 0.0;
}


(* ok *)
let evaluateDiffuse self aMaterial aLocalDirGen oDirectPdfW oReversePdfW =
  let null_eval () = {
    null_evaluating with 
    oDirectPdfW;
    oReversePdfW;
  } in
  if self.mProbabilities.diffProb = 0.
  then null_eval ()
  else 
    let localDirFixZ = V.get self.mLocalDirFix 2
    and localDirGenZ = V.get aLocalDirGen 2 in
    if localDirFixZ < eps_cosine || localDirGenZ < eps_cosine
    then null_eval ()
    else
      let computeDiffuse pdfW z =
        pdfW +. self.mProbabilities.diffProb *. (max 0. @@ z *. inv_pi) in
      let oDirectPdfW = computeDiffuse oDirectPdfW  localDirGenZ
      and oReversePdfW = computeDiffuse oReversePdfW localDirFixZ in
      { 
        oDirectPdfW;
        oReversePdfW;
        oBSDF = V.scale (Material.diffuseReflectance aMaterial) inv_pi;
      }

(* ok *)
let evaluatePhong self aMaterial aLocalDirGen oDirectPdfW oReversePdfW =
  let null_eval () = {
    null_evaluating with 
    oDirectPdfW;
    oReversePdfW;
  } in
  if self.mProbabilities.phongProb = 0.
  then null_eval ()
  else 
    let mLocalDirFixZ = V.get self.mLocalDirFix 2
    and aLocalDirGenZ = V.get aLocalDirGen 2 in
    if mLocalDirFixZ < eps_cosine || aLocalDirGenZ < eps_cosine
    then null_eval ()
    else 
      (* assumes this is never called when rejectShadingCos(oLocalDirGen.z) is true *)
      let reflLocalDirIn = reflectLocal self.mLocalDirFix in
      let dot_R_Wi = V.dot reflLocalDirIn aLocalDirGen in
      if dot_R_Wi <= eps_phong
      then null_eval ()
      else
        (* the sampling is symmetric *)
        let exponent = Material.phongExponent aMaterial
        and reflectance = Material.phongReflectance aMaterial in
        let pdfW = self.mProbabilities.phongProb
                   *. powerCosHemispherePdfW reflLocalDirIn aLocalDirGen exponent in
        let oDirectPdfW  = oDirectPdfW +. pdfW
        and oReversePdfW = oReversePdfW +. pdfW
        and rho = V.scale reflectance (((exponent +. 2.) *. 0.5 *. inv_pi) *. (dot_R_Wi ** exponent)) in
        { 
          oDirectPdfW;
          oReversePdfW;
          oBSDF = rho;
        }

(* \brief Given a direction, evaluates BSDF
 *
 * Returns value of BSDF, as well as cosine for the
 * aWorldDirGen direction.
 * Can return probability (w.r.t. solid angle W),
 * of having sampled aWorldDirGen given mLocalDirFix (oDirectPdfW),
 * and of having sampled mLocalDirFix given aWorldDirGen (oReversePdfW).
 *
 *)
(* ok *)
let evaluate self aScene aWorldDirGen : evaluate_result =
  let oLocalDirGen = Frame.toLocal self.mFrame aWorldDirGen in
  let oLocalDirGenZ = V.get oLocalDirGen 2 in
  if oLocalDirGenZ *. (V.get self.mLocalDirFix 2) < 0.
  then null_eval oLocalDirGen
  else
    let oCosThetaGen = abs_float oLocalDirGenZ
    and mat = Scene.getMaterial aScene self.mMaterialID in
    let r1 = evaluateDiffuse self mat oLocalDirGen 0. 0. in
    let r2 = evaluatePhong self mat oLocalDirGen r1.oDirectPdfW r1.oReversePdfW in
    { 
      oDirectPdfW = r2.oDirectPdfW;
      oReversePdfW = r2.oReversePdfW;
      oBSDF = V.add r1.oBSDF r2.oBSDF;
      oLocalDirGen;
      oCosThetaGen 
    }


(*//////////////////////////////////////////////////////////////////////////
 * Albedo methods
 *////////////////////////////////////////////////////////////////////////*)
          
let albedoDiffuse aMaterial =
  luminance @@ Material.diffuseReflectance aMaterial

let albedoPhong aMaterial =
  luminance @@ Material.phongReflectance aMaterial

let albedoReflect aMaterial =
  luminance @@ Material.mirrorReflectance aMaterial

let albedoRefract aMaterial =
  if Material.ior aMaterial > 0. then 1. else 0.


(* \brief Given 3 random numbers, samples new direction from BSDF.
 *
 * Uses z component of random triplet to pick BSDF component from
 * which it will sample direction. If non-specular component is chosen,
 * it will also evaluate the other (non-specular) BSDF components.
 * Return BSDF factor for given direction, as well as PDF choosing that direction.
 * Can return event which has been sampled.
 * If result is Vec3f(0,0,0), then the sample should be discarded.
 *)


type sample_result = {
  oBSDF         : V.t;
  oWorldDirGen  : V.t;
  oPdfW         : float;
  oCosThetaGen  : float;
  oSampledEvent : events;
}

let null_sample = {
  oBSDF         = V.null ();
  oWorldDirGen  = V.null ();
  oPdfW         = 0.;
  oCosThetaGen  = 0.;
  oSampledEvent = NONE;
}

(* ok *)
let sample self aScene aRndTriplet =
  let oSampledEvent =
    let aRndTripletZ = V.get aRndTriplet 2 in
    if aRndTripletZ < self.mProbabilities.diffProb
    then Diffuse
    else if aRndTripletZ < self.mProbabilities.diffProb +. self.mProbabilities.phongProb
    then Phong
    else if aRndTripletZ < self.mProbabilities.diffProb +. self.mProbabilities.phongProb +. self.mProbabilities.reflProb
    then Reflect
    else Refract
  in
  let mat = Scene.getMaterial aScene self.mMaterialID in
  let aRndXY = let x,y,z = tuple_of_v3 aRndTriplet in V2f.of_tuple (x,y) in
  let smpl =
    match oSampledEvent with
      Diffuse -> let smpl = sampleDiffuse self mat aRndXY 0. in 
                 if notZero smpl.oBSDF
                 then let ev = evaluatePhong self mat smpl.oLocalDirGen smpl.oPdfW smpl.oPdfW in
                      { smpl with
                        oPdfW        = ev.oDirectPdfW;
                        oBSDF        = V.add ev.oBSDF smpl.oBSDF
                      }
                 else smpl
    | Phong   -> let smpl = samplePhong self mat aRndXY 0. in 
                 if notZero smpl.oBSDF
                 then let ev = evaluateDiffuse self mat smpl.oLocalDirGen smpl.oPdfW smpl.oPdfW in
                      { smpl with 
                        oPdfW        = ev.oDirectPdfW;
                        oBSDF        = V.add ev.oBSDF smpl.oBSDF
                      }
                 else smpl
    | Reflect -> sampleReflect self mat aRndXY 0.
    | Refract -> sampleRefract self mat aRndXY 0.
    | _       -> assert false
  in
  let oCosThetaGen   = abs_float (V.get smpl.oLocalDirGen 2) in
  if oCosThetaGen < eps_cosine
  then {oBSDF = V.null (); oWorldDirGen = V.null (); oPdfW = smpl.oPdfW; oCosThetaGen; oSampledEvent }
  else let oWorldDirGen = Frame.toWorld self.mFrame smpl.oLocalDirGen in
       {oBSDF = smpl.oBSDF; oWorldDirGen; oPdfW = smpl.oPdfW; oCosThetaGen; oSampledEvent }


(* constructor *)
(* ok *)
let getComponentProbabilities self aMaterial (* oProbabilities *) =
  debug "getComponentProbabilities : %a %a\n" dump self Material.dump aMaterial;
  let mReflectCoeff = fresnelDielectric (V.get self.mLocalDirFix 2) (Material.ior aMaterial) in
  debug "  mReflectCoeff = %.03e\n" mReflectCoeff;
  let albedoDiffuse = albedoDiffuse aMaterial
  and albedoPhong   = albedoPhong aMaterial
  and albedoReflect = mReflectCoeff         *. albedoReflect aMaterial
  and albedoRefract = (1. -. mReflectCoeff) *. albedoRefract aMaterial in
  debug "  albedos = %.03e %.03e %.03e %.03e\n" albedoDiffuse albedoPhong albedoReflect albedoRefract;
  let totalAlbedo = albedoDiffuse +. albedoPhong +. albedoReflect +. albedoRefract in
  let mContinuationProb, probs =
    if totalAlbedo < 1e-9
    then 0., nullProbabilities
    else
    let probs = {
            diffProb  = albedoDiffuse /. totalAlbedo;
            phongProb = albedoPhong   /. totalAlbedo;
            reflProb  = albedoReflect /. totalAlbedo;
            refrProb  = albedoRefract /. totalAlbedo;
    }
    (* The continuation probability is max component from reflectance.
     * That way the weight of sample will never rise.
     * Luminance is another very valid option. *)
    and mContinuationProb =
      let c = 
        let v =  (V.add
                    (Material.diffuseReflectance aMaterial)
                    @@ V.add (Material.phongReflectance aMaterial)
                             (V.scale (Material.mirrorReflectance aMaterial) mReflectCoeff))
        in (cmax v) +. (1. -. mReflectCoeff) in
      clamp 0. 1. c
    in mContinuationProb, probs in
  { self with mContinuationProb; mReflectCoeff }, probs

(* ok *)
let setup mFixIsLight aRay aIsect aScene =
  let mFrame = Frame.setFromZ (Isect.normal aIsect) in
  debug "BSDF<%b>::Setup()\n" mFixIsLight;
  debug "  aRay = %a\n" Ray.dump aRay;
  debug "  aIsect = %a\n" Isect.dump aIsect;
  debug "  mFrame = %a\n" Frame.dump mFrame;
  let mLocalDirFix = Frame.toLocal mFrame (V.opp @@ Ray.dir aRay) in
  debug "  mLocalDirFix = %a\n" pprintf_v mLocalDirFix;
  (* reject rays that are too parallel with tangent plane *)
  let self = { (invalid ()) with mFrame; mLocalDirFix } in
  if (abs_float @@ V.get mLocalDirFix 2) < eps_cosine
  then self
  else 
    let mat = Scene.getMaterial aScene @@ Isect.matID aIsect in
    debug "  mat = %a\n" Material.dump mat;
    let self, mProbabilities = getComponentProbabilities self mat in
    let mIsDelta = (mProbabilities.diffProb = 0.) && (mProbabilities.phongProb = 0.) in
    (* now it becomes valid *)
    let mMaterialID = Isect.matID aIsect in
    { self with mIsDelta; mProbabilities; mMaterialID; mFixIsLight }

let make mFixIsLight aRay aIsect aScene =
  setup mFixIsLight aRay aIsect aScene
