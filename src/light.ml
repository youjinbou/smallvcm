open Utils

type illuminate_result = {
  oDirectionToLight : V.t;
  oDistance         : float;
  oDirectPdfW       : float;
  oEmissionPdfW     : float option;
  oCosAtLight       : float option;
  radiance          : V.t;
}

type emit_result = {
  oPosition      : V.t;
  oDirection     : V.t;
  oEmissionPdfW  : float;
  oDirectPdfA    : float option;
  oCosThetaLight : float option;
  energy         : V.t;
}

module type Abstract = sig

    type t

    (* \brief Illuminates a given point in the scene.
     *
     * Given a point and two random samples (e.g., for position on area lights),
     * this method returns direction from point to light, distance,
     * pdf of having chosen this direction (e.g., 1 / area).
     * Optionally also returns pdf of emitting particle in this direction,
     * and cosine from lights normal (helps with PDF of hitting the light,
     * but set to 1 for point lights).
     *
     * Returns radiance.
     *)
    val illuminate :
        self               : t ->
        aSceneSphere       : SceneSphere.t ->
        aReceivingPosition : V.t ->
        aRndTuple          : V2f.t -> 
        emissionPdfW       : bool ->
        cosAtLight         : bool -> illuminate_result

    (* \brief Emits particle from the light.
     *
     * Given two sets of random numbers (e.g., position and direction on area light),
     * this method generates a position and direction for light particle, along
     * with the pdf.
     *
     * Can also supply pdf (w.r.t. area) of choosing this position when calling
     * Illuminate. Also provides cosine on the light (this is 1 for point lights etc.).
     *
     * Returns "energy" that particle carries
     *)

    val emit :
      self          : t ->
      aSceneSphere  : SceneSphere.t ->
      aDirRndTuple  : V2f.t ->
      aPosRndTuple  : V2f.t ->
      directPdfA    : bool -> 
      cosAtLight    : bool -> emit_result

    (* \brief Returns radiance for ray randomly hitting the light
     *
     * Given ray direction and hitpoint, it returns radiance.
     * Can also provide area pdf of sampling hitpoint in Illuminate,
     * and of emitting particle along the ray (in opposite direction).
     *)

    val getRadiance :
      self          : t ->
      aSceneSphere  : SceneSphere.t ->
      aRayDirection : V.t ->
      aHitPoint     : V.t -> (V.t * float)

    (* \brief Returns radiance for ray randomly hitting the light
     *
     * Given ray direction and hitpoint, it returns radiance.
     * Can also provide area pdf of sampling hitpoint in Illuminate,
     * and of emitting particle along the ray (in opposite direction).
     *)

    val getRadianceEmission :
      self          : t ->
      aSceneSphere  : SceneSphere.t ->
      aRayDirection : V.t ->
      aHitPoint     : V.t -> (V.t * float * float)

    (* Whether the light has a finite extent (area, point) or not (directional, env. map) *)
    val isFinite : t -> bool

    (* Whether the light has delta function (point, directional) or not (area) *)
    val isDelta : t -> bool

end

(* ////////////////////////////////////////////////////////////////////////// *)

module AreaLight = struct

  type t = {
    p0 : V.t;
    e1 : V.t;
    e2 : V.t;
    mFrame : Frame.t;
    mIntensity: V.t;
    mInvArea : float;
  }

  let make aP0 aP1 aP2 i =
    let p0 = aP0
    and e1 = V.sub aP1 aP0
    and e2 = V.sub aP2 aP0 in
    let normal = cross e1 e2 in
    let len = V.length normal in
    let mInvArea = 2. /. len in {
      p0;
      e1;
      e2;
      mFrame = Frame.setFromZ normal;
      mIntensity = i;
      mInvArea;
    }

  let illuminate self aSceneSphere aReceivingPosition aRndTuple emissionPdfW cosAtLight =
    let uv = sampleUniformTriangle aRndTuple in
    let lightPoint = V.add self.p0 
                          @@ V.add  (V.scale self.e1 (V2f.get uv 0)) 
                                    (V.scale self.e2 (V2f.get uv 1)) in
    let oDirectionToLight   = V.sub lightPoint aReceivingPosition in
    let distSqr             = lenSqr oDirectionToLight in
    let oDistance           = sqrt distSqr in
    let oDirectionToLight   = V.scale oDirectionToLight (1. /. oDistance) in
    let cosNormalDir        = -. (V.dot (Frame.normal self.mFrame) oDirectionToLight) in
    (* too close to, or under, tangent *)
    if cosNormalDir < eps_cosine
    then {
      oDirectionToLight;
      oDistance;
      oDirectPdfW = 0.0;
      oEmissionPdfW = if emissionPdfW then Some 0.0 else None;
      oCosAtLight = if cosAtLight then Some 0.0 else None;
      radiance = V.null ();
    }
    else
      let oDirectPdfW = self.mInvArea *. distSqr /. cosNormalDir in
      let oCosAtLight =
        if cosAtLight then Some cosNormalDir else None
      and oEmissionPdfW = 
        if emissionPdfW then Some (self.mInvArea *. cosNormalDir *. inv_pi) else None
      in {
        oDirectionToLight;
        oDistance;
        oDirectPdfW;
        oEmissionPdfW;
        oCosAtLight;
        radiance = self.mIntensity;
      }

    let emit self aSceneSphere aDirRndTuple aPosRndTuple directPdfA cosThetaLight =
      let uv = sampleUniformTriangle aPosRndTuple in
      let uvx, uvy = V2f.to_tuple uv in
      let oPosition = V.add self.p0 @@ V.add (V.scale self.e1 uvx) (V.scale self.e2 uvy) in
      let oEmissionPdfW, localDirOut = sampleCosHemisphereW aDirRndTuple in
      (* cannot really not emit the particle, so just bias it to the correct angle *)
      let localDirOutZ = max (V.get localDirOut 2) eps_cosine in
      let localDirOut = V.set localDirOut 2 localDirOutZ in
      {
        oPosition;
        oDirection     = Frame.toWorld self.mFrame localDirOut;
        oEmissionPdfW  = oEmissionPdfW *. self.mInvArea;
        oDirectPdfA    = if directPdfA then Some self.mInvArea else None;
        oCosThetaLight = if cosThetaLight then Some localDirOutZ else None;
        energy         = V.scale self.mIntensity localDirOutZ;
      }

    let getRadiance self aSceneSphere aRayDirection aHitPoint =
      let fnormal = Frame.normal self.mFrame
      and rayDirectionOpp = V.opp aRayDirection in
      let cosOutL = max 0. (V.dot fnormal rayDirectionOpp) in
      if cosOutL = 0.0
      then V.null (), 0.
      else self.mIntensity, self.mInvArea

    let getRadianceEmission self aSceneSphere aRayDirection aHitPoint =
      let fnormal = Frame.normal self.mFrame
      and rayDirectionOpp = V.opp aRayDirection in
      let cosOutL = max 0. (V.dot fnormal rayDirectionOpp) in
      if cosOutL = 0.0
      then V.null (), 0., 0.
      else let o = cosHemispherePdfW fnormal rayDirectionOpp in
           self.mIntensity, self.mInvArea, self.mInvArea *. o

    (* Whether the light has a finite extent (area, point) or not (directional, env. map) *)
    let isFinite self = true

    (* Whether the light has delta function (point, directional) or not (area) *)
    let isDelta self = false

end


(* ////////////////////////////////////////////////////////////////////////// *)
module DirectionalLight = struct

  type t = {
    mFrame : Frame.t;
    mIntensity : V.t;
  }

  let make aDirection i = {
    mFrame = Frame.setFromZ aDirection;
    mIntensity = i;
  }

  let illuminate
        self
        aSceneSphere
        aReceivingPosition
        aRndTuple
        emissionPdfW
        cosAtLight =
    {
      oDirectionToLight     = V.opp (Frame.normal self.mFrame);
      oDistance             = 1e36;
      oDirectPdfW           = 1.;
      oCosAtLight           = if cosAtLight then Some 1. else None;
      oEmissionPdfW         = 
        if cosAtLight 
        then Some (concentricDiscPdfA () *. (SceneSphere.invSceneRadiusSqr aSceneSphere))
        else None;
      radiance              = self.mIntensity;
    }

  let emit
        self
        aSceneSphere
        aDirRndTuple
        aPosRndTuple
        directPdfA
        cosThetaLight =

    let xy = sampleConcentricDisc aPosRndTuple 
    and fnormal = Frame.normal self.mFrame in {
      oPosition =
        V.add (SceneSphere.sceneCenter aSceneSphere)
        @@ V.scale 
             (V.add (V.scale fnormal (-1.))
                   @@ V.add  (V.scale (Frame.binormal self.mFrame) (V2f.get xy 0))
                             (V.scale (Frame.tangent self.mFrame)  (V2f.get xy 1)))
             (SceneSphere.sceneRadius aSceneSphere);

      oDirection = fnormal;
      oEmissionPdfW = concentricDiscPdfA () *. (SceneSphere.invSceneRadiusSqr aSceneSphere);
      oDirectPdfA = if directPdfA then Some 1. else None;

      (* Not used for infinite or delta lights *)
      oCosThetaLight = if cosThetaLight then Some 1. else None;
      energy = self.mIntensity;
    }

  let getRadiance
        self
        aSceneSphere
        aRayDirection
        aHitPoint
    = V.null (), 0.

  let getRadianceEmission
        self
        aSceneSphere
        aRayDirection
        aHitPoint
    = V.null (), 0., 0.

  (* Whether the light has a finite extent (area, point) or not (directional, env. map) *)
  let isFinite self = false
    
  (* Whether the light has delta function (point, directional) or not (area) *)
  let isDelta self = true

end

(* ////////////////////////////////////////////////////////////////////////// *)
module PointLight = struct

  type t = {
    mPosition : V.t;
    mIntensity : V.t;
  }

  let make aPosition i = {
    mPosition  = aPosition;
    mIntensity = i;
  }

  let illuminate
        self
        aSceneSphere
        aReceivingPosition
        aRndTuple
        emissionPdfW
        cosAtLight =
    let oDirectionToLight  = V.sub self.mPosition aReceivingPosition in
    let distSqr            = lenSqr oDirectionToLight in
    let oDistance          = sqrt distSqr in
    {
      oDirectionToLight  = V.scale oDirectionToLight ( 1. /. oDistance);
      oDirectPdfW        = distSqr;
      oDistance;
      oCosAtLight        = if cosAtLight then Some 1. else None;
      oEmissionPdfW      = if emissionPdfW then Some (uniformSpherePdfW ()) else None;
      radiance           = self.mIntensity;
    }

  let emit
        self
        aSceneSphere
        aDirRndTuple
        aPosRndTuple
        directPdfA
        cosThetaLight = 
    let oPosition  = self.mPosition
    and oEmissionPdfW, oDirection = sampleUniformSphereW aDirRndTuple in
    {
      oPosition;
      oDirection;
      oEmissionPdfW;
      oDirectPdfA =  if directPdfA then Some 1. else None;
      (* Not used for infinite or delta lights *)
      oCosThetaLight = if cosThetaLight then Some 1. else None;
      energy = self.mIntensity;
    }

  let getRadiance
        self
        aSceneSphere
        aRayDirection
        aHitPoint
    = V.null (), 0.

  let getRadianceEmission
        self
        aSceneSphere
        aRayDirection
        aHitPoint
    = V.null (), 0., 0.

    
    (* Whether the light has a finite extent (area, point) or not (directional, env. map) *)
    let isFinite self = true

    (* Whether the light has delta function (point, directional) or not (area) *)
    let isDelta self = true

end


(* ////////////////////////////////////////////////////////////////////////// *)
module BackgroundLight  = struct

  type t = {
    mBackgroundColor : V.t;
    mScale : float;
  }

  let make s = {
    mBackgroundColor = V.mul (v3_of_tuple (135., 206., 250.)) (V.make @@ 1. /. 255.);
    mScale = s;
  }

  let illuminate 
        self
        aSceneSphere
        aReceivingPosition
        aRndTuple
        emissionPdfW
        cosAtLight =
    
    (* Replace these two lines with image sampling *)
    let oDirectPdfW, oDirectionToLight = sampleUniformSphereW aRndTuple

    (* oDirectionToLight = Vec3f(0.16123600f, -0.98195398f, 0.098840252f); *)
    and radiance = V.scale self.mBackgroundColor self.mScale

    (* This stays even with image sampling *)
    and oDistance = 1e36 in
    { 
      oDirectionToLight;
      oDistance;
      oDirectPdfW;
      oEmissionPdfW =
        if emissionPdfW
        then Some (oDirectPdfW
                   *. (concentricDiscPdfA ())
                   *. (SceneSphere.invSceneRadiusSqr aSceneSphere))
        else None;
      oCosAtLight = if cosAtLight then Some 1. else None;
      radiance;
    }

  let emit
        self
        aSceneSphere
        aDirRndTuple
        aPosRndTuple
        directPdfA
        cosThetaLight =

    (* Replace these two lines with image sampling *)
    let directPdf, oDirection = sampleUniformSphereW aDirRndTuple 
    (* oDirection = V.of_tuple (-0.16123600, 0.98195398, -0.098840252) *)
    and radiance   = V.scale self.mBackgroundColor self.mScale
    (* Stays even with image sampling *)
    and xy = sampleConcentricDisc aPosRndTuple in

    let frame = Frame.setFromZ oDirection in
    {
      oPosition = V.add (V.add (SceneSphere.sceneCenter aSceneSphere) 
                               (V.scale oDirection (-.(SceneSphere.sceneRadius aSceneSphere))))
                        (V.add (V.scale (Frame.binormal frame) (V2f.get xy 0))
                               (V.scale (Frame.tangent frame) (V2f.get xy 1)));
      
      (* oPosition = Vec3f(-1.109054f, -2.15064538f, -1.087019148f); *)
      oDirection;
      oEmissionPdfW =
        directPdf 
        *. (concentricDiscPdfA ())
        *. (SceneSphere.invSceneRadiusSqr aSceneSphere);

      (* For background we lie about Pdf being in area measure *)
      oDirectPdfA = if directPdfA then Some directPdf else None;
      (* Not used for infinite or delta lights *)
      oCosThetaLight = if cosThetaLight then Some 1. else None;
      energy = radiance;
    }

  let getRadiance
        self
        aSceneSphere
        aRayDirection
        aHitPoint =
    (* Replace this with image lookup (proper pdf and such) *)
    (* use aRayDirection *)
    let directPdf   = uniformSpherePdfW ()
    and radiance    = V.scale self.mBackgroundColor self.mScale
    in radiance, directPdf

  let getRadianceEmission
        self
        aSceneSphere
        aRayDirection
        aHitPoint =
    (* Replace this with image lookup (proper pdf and such) *)
    (* use aRayDirection *)
    let directPdf   = uniformSpherePdfW ()
    and radiance    = V.scale self.mBackgroundColor self.mScale
    and positionPdf = concentricDiscPdfA () *. (SceneSphere.invSceneRadiusSqr aSceneSphere) 
    in radiance, directPdf , directPdf *. positionPdf

  (* Whether the light has a finite extent (area, point) or not (directional, env. map) *)
  let isFinite self = false
                        
  (* Whether the light has delta function (point, directional) or not (area) *)
  let isDelta self = false

end

type t = 
  | AreaLight of AreaLight.t
  | DirectionalLight of DirectionalLight.t
  | PointLight of PointLight.t
  | BackgroundLight of BackgroundLight.t

let illuminate = function
  | AreaLight l ->  AreaLight.illuminate l
  | DirectionalLight l -> DirectionalLight.illuminate l
  | PointLight l -> PointLight.illuminate l
  | BackgroundLight l -> BackgroundLight.illuminate l

let emit = function
  | AreaLight l ->  AreaLight.emit l
  | DirectionalLight l -> DirectionalLight.emit l
  | PointLight l -> PointLight.emit l
  | BackgroundLight l -> BackgroundLight.emit l

let getRadiance  = function
  | AreaLight l ->  AreaLight.getRadiance l
  | DirectionalLight l -> DirectionalLight.getRadiance l
  | PointLight l -> PointLight.getRadiance l
  | BackgroundLight l -> BackgroundLight.getRadiance l

let getRadianceEmission = function
  | AreaLight l ->  AreaLight.getRadianceEmission l
  | DirectionalLight l -> DirectionalLight.getRadianceEmission l
  | PointLight l -> PointLight.getRadianceEmission l
  | BackgroundLight l -> BackgroundLight.getRadianceEmission l

let isFinite = function
  | AreaLight l ->  AreaLight.isFinite l
  | DirectionalLight l -> DirectionalLight.isFinite l
  | PointLight l -> PointLight.isFinite l
  | BackgroundLight l -> BackgroundLight.isFinite l

let isDelta = function
  | AreaLight l ->  AreaLight.isDelta l
  | DirectionalLight l -> DirectionalLight.isDelta l
  | PointLight l -> PointLight.isDelta l
  | BackgroundLight l -> BackgroundLight.isDelta l

