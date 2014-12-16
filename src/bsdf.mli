val eps_phong : float
type truetype
type falsetype
type componentProbabilities = {
  diffProb : float;
  phongProb : float;
  reflProb : float;
  refrProb : float;
}
val nullProbabilities : componentProbabilities
type events =
    NONE
  | Diffuse
  | Phong
  | Reflect
  | Refract
(*
  | Specular
  | NonSpecular
  | All *)
type t = {
  mMaterialID : int;
  mFrame : Frame.t;
  mLocalDirFix : Utils.V.t;
  mIsDelta : bool;
  mProbabilities : componentProbabilities;
  mContinuationProb : float;
  mReflectCoeff : float;
  mFixIsLight : bool;
}
val invalid : unit -> t
val isValid : t -> bool
val isDelta : t -> bool
val continuationProb : t -> float
val cosThetaFix : t -> Utils.V.scalar
val worldDirFix : t -> Utils.V.t
val pdfDiffuse : t -> 'b -> Utils.V.t -> float -> float -> float * float
val pdfPhong :
  t -> Material.t -> Utils.V.t -> float -> float -> float * float
val pdf : t -> ?aEvalRevPdf:bool -> 'b Scene.t -> Utils.V.t -> float
type samplingMethod_result = {
  oBSDF : Utils.V.t;
  oLocalDirGen : Utils.V.t;
  oPdfW : float;
}
val null_sampling : samplingMethod_result
val sampleDiffuse :
  t -> Material.t -> Utils.V2f.t -> float -> samplingMethod_result
val samplePhong :
  t -> Material.t -> Utils.V2f.t -> float -> samplingMethod_result
val sampleReflect :
  t -> Material.t -> 'b -> float -> samplingMethod_result
val sampleRefract :
  t -> Material.t -> 'b -> float -> samplingMethod_result

type evaluating = {
  oDirectPdfW : float;
  oReversePdfW : float;
  oBSDF : Utils.V.t;
}

type evaluate_result = {
  oDirectPdfW : float;
  oReversePdfW : float;
  oBSDF : Utils.V.t;
  oLocalDirGen : Utils.V.t;
  oCosThetaGen : float;
}

val null_eval : Utils.V.t -> evaluate_result
val evaluateDiffuse :
  t -> Material.t -> Utils.V.t -> float -> float -> evaluating
val evaluatePhong :
  t -> Material.t -> Utils.V.t -> float -> float -> evaluating
val evaluate : t -> 'b Scene.t -> Utils.V.t -> evaluate_result
val albedoDiffuse : Material.t -> float
val albedoPhong : Material.t -> float
val albedoReflect : Material.t -> float
val albedoRefract : Material.t -> float
type sample_result = {
  oBSDF : Utils.V.t;
  oWorldDirGen : Utils.V.t;
  oPdfW : float;
  oCosThetaGen : float;
  oSampledEvent : events;
}
val null_sample : sample_result
val sample : t -> 'b Scene.t -> Utils.V.t -> sample_result
val getComponentProbabilities : t -> Material.t -> t * componentProbabilities
val setup : bool -> Ray.t -> Isect.t -> 'a Scene.t -> t
val make : bool -> Ray.t -> Isect.t -> 'a Scene.t -> t
