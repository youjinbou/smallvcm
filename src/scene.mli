open Utils

type boxMask =
    LightCeiling
  | LightSun
  | LightPoint
  | LightBackground
  | LargeMirrorSphere
  | LargeGlassSphere
  | SmallMirrorSphere
  | SmallGlassSphere
  | GlossyFloor

module IntMap :
sig
  type key = int
  type +'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val choose : 'a t -> key * 'a
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

type 'a t = {
  mGeometry : Geometry.t;
  mCamera : Camera.t;
  mMaterials : Material.t array;
  mLights : Light.t array;
  mMaterial2Light : int IntMap.t;
  mSphere : 'a;
  mBackground : Light.BackgroundLight.t option;
  mSceneName : string;
  mSceneAcronym : string;
}

val geometry : 'a t -> Geometry.t
val camera : 'a t -> Camera.t
val materials : 'a t -> Material.t array
val lights : 'a t -> Light.t array
val material2Light : 'a t -> int IntMap.t
val sphere : 'a t -> 'a
val background : 'a t -> Light.BackgroundLight.t option
val sceneName : 'a t -> string
val sceneAcronym : 'a t -> string
val buildSphere : 'a t -> SceneSphere.t t
val ( & ) : 'a list -> 'a -> bool
val intersect : 'a t -> Ray.t -> Isect.t -> Isect.t option
val occluded : 'a t -> V.t -> V.t -> float -> bool
val getMaterial : 'a t -> int -> Material.t
val getMaterialCount : 'a t -> int
val getLightPtr : 'a t -> int -> Light.t
val getLightCount : 'a t -> int
val getBackground : 'a t -> Light.BackgroundLight.t option
val getSceneName : boxMask list -> string * string
val cornellBox : V2i.t -> boxMask list -> unit t
