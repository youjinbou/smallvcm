open Utils

type illuminate_result = {
  oDirectionToLight : V.t;
  oDistance : float;
  oDirectPdfW : float;
  oEmissionPdfW : float option;
  oCosAtLight : float option;
  radiance : V.t;
}
type emit_result = {
  oPosition : V.t;
  oDirection : V.t;
  oEmissionPdfW : float;
  oDirectPdfA : float option;
  oCosThetaLight : float option;
  energy : V.t;
}

module type Abstract =
  sig
    type t
    val illuminate :
      self:t ->
      aSceneSphere:SceneSphere.t ->
      aReceivingPosition:V.t ->
      aRndTuple:V2f.t ->
      emissionPdfW:bool -> cosAtLight:bool -> illuminate_result
    val emit :
      self:t ->
      aSceneSphere:SceneSphere.t ->
      aDirRndTuple:V2f.t ->
      aPosRndTuple:V2f.t -> directPdfA:bool -> cosAtLight:bool -> emit_result
    val getRadiance :
      self:t ->
      aSceneSphere:SceneSphere.t ->
      aRayDirection:V.t -> aHitPoint:V.t -> V.t * float
    val getRadianceEmission :
      self:t ->
      aSceneSphere:SceneSphere.t ->
      aRayDirection:V.t ->
      aHitPoint:V.t -> V.t * float * float
    val isFinite : t -> bool
    val isDelta : t -> bool

    val dump : out_channel -> t -> unit

  end

module AreaLight :
  sig
    type t = {
      p0 : V.t;
      e1 : V.t;
      e2 : V.t;
      mFrame : Frame.t;
      mIntensity : V.t;
      mInvArea : float;
    }
    val make : V.t -> V.t -> V.t -> V.t -> t
    val illuminate :
      t ->
      'a -> V.t -> V2f.t -> bool -> bool -> illuminate_result
    val emit :
      t -> 'a -> V2f.t -> V2f.t -> bool -> bool -> emit_result
    val getRadiance : t -> 'a -> V.t -> 'b -> V.t * float
    val getRadianceEmission :
      t -> 'a -> V.t -> 'b -> V.t * float * float
    val isFinite : 'a -> bool
    val isDelta : 'a -> bool
  end

module DirectionalLight :
  sig
    type t = { mFrame : Frame.t; mIntensity : V.t; }
    val make : V.t -> V.t -> t
    val illuminate :
      t -> SceneSphere.t -> 'a -> 'b -> 'c -> bool -> illuminate_result
    val emit :
      t -> SceneSphere.t -> 'a -> V2f.t -> bool -> bool -> emit_result
    val getRadiance : 'a -> 'b -> 'c -> 'd -> V.t * float
    val getRadianceEmission :
      'a -> 'b -> 'c -> 'd -> V.t * float * float
    val isFinite : 'a -> bool
    val isDelta : 'a -> bool
  end

module PointLight :
  sig
    type t = { mPosition : V.t; mIntensity : V.t; }
    val make : V.t -> V.t -> t
    val illuminate :
      t -> 'a -> V.t -> 'b -> bool -> bool -> illuminate_result
    val emit : t -> 'a -> V2f.t -> 'b -> bool -> bool -> emit_result
    val getRadiance : 'a -> 'b -> 'c -> 'd -> V.t * float
    val getRadianceEmission :
      'a -> 'b -> 'c -> 'd -> V.t * float * float
    val isFinite : 'a -> bool
    val isDelta : 'a -> bool
  end

module BackgroundLight :
  sig
    type t = { mBackgroundColor : V.t; mScale : float; }
    val make : float -> t
    val illuminate :
      t ->
      SceneSphere.t -> 'a -> V2f.t -> bool -> bool -> illuminate_result
    val emit :
      t ->
      SceneSphere.t ->
      V2f.t -> V2f.t -> bool -> bool -> emit_result
    val getRadiance : t -> SceneSphere.t -> 'a -> 'b -> V.t * float
    val getRadianceEmission :
      t -> SceneSphere.t -> 'a -> 'b -> V.t * float * float
    val isFinite : 'a -> bool
    val isDelta : 'a -> bool
  end

type t =
    AreaLight of AreaLight.t
  | DirectionalLight of DirectionalLight.t
  | PointLight of PointLight.t
  | BackgroundLight of BackgroundLight.t

val illuminate :
  t ->
  SceneSphere.t ->
  V.t -> V2f.t -> bool -> bool -> illuminate_result

val emit :
  t ->
  SceneSphere.t -> V2f.t -> V2f.t -> bool -> bool -> emit_result

val getRadiance : t -> SceneSphere.t -> V.t -> 'a -> V.t * float

val getRadianceEmission :
  t -> SceneSphere.t -> V.t -> 'a -> V.t * float * float

val isFinite : t -> bool

val isDelta : t -> bool

val dump : out_channel -> t -> unit
