let (??) x = if x then 1 else 0

let debug fmt = Printf.ifprintf stderr fmt


module GV = struct

  module V  = struct

    include Gmaths.Vector
                                                           
    let set v i k = set v i k; v 

  end
  module V3i : Gmaths.Vec.S with type tuple_t = int * int * int
                             and type Scalar.t = int = Gmaths.Vec3.Int
  module V2f : Gmaths.Vec.S with type tuple_t = float * float
                             and type Scalar.t = float = Gmaths.Vec2.Float
  module V2i : Gmaths.Vec.S with type tuple_t = int * int
                             and type Scalar.t = int = Gmaths.Vec2.Int
  module M   = Gmaths.Matrix.Mat4
  module V4  = Gmaths.Vec4.Float

  let cmax v =
    let x,y,z,_ = V.to_tuple v in
    max x (max y z)

  let v2lenSqr v = V2f.dot v v
  let lenSqr v = V.dot v v

  let tuple_of_v3 v =
    let x, y, z, _ = V.to_tuple v in
    x, y, z

  let v3_of_tuple (x,y,z) =
    V.of_tuple (x,y,z,1.)

  let v4_of_v3 v =
    let x, y, z, _ = V.to_tuple v in
    V4.of_tuple (x,y,z,1.)

  let v3_of_v4 v =
    let x, y, z, _ = V.to_tuple v in
    V.of_tuple (x,y,z,1.)

  let v3_of_v2 v z =
    let x, y = V2f.to_tuple v in
    V.of_tuple (x, y, z, 1.)

  let v2i_of_v3 v =
    let x, y, _, _ = V.to_tuple v in
    V2i.of_tuple (int_of_float x, int_of_float y)

  let cross a b =
    V.cross [| a ; b |]

  let m_apply3 m v =
    v3_of_v4 @@ M.apply m @@ v4_of_v3 v

  let m_apply4 m v =
    M.apply m v

  let vmin = V.min
  let vmax = V.max

end

module MV = struct
  open Math
  module V = V3f
  module V3i = Math.V3i
  module V2f = Math.V2f
  module V2i = Math.V2i
  module M = Math.M
  module V4 = V4f

  let vmax v1 v2 =
    V.map2 max v1 v2

  let vmin v1 v2 =
    V.map2 min v1 v2

  let cmax = V.max

  let v2lenSqr v = V2f.dot v v
  let lenSqr v = V3f.lenSqr v

  let tuple_of_v3 v =
    V.to_tuple v

  let v3_of_tuple (x,y,z) =
    V.of_tuple (x,y,z)

  let v3_of_v4 = Math.v3_of_v4

  let v4_of_v3 = Math.v4_of_v3

  let v3_of_v2 v z =
    let x, y = V2f.to_tuple v in
    v3_of_tuple (x, y, z)

  let v2i_of_v3 v =
    let x, y, _= tuple_of_v3 v in
    V2i.of_tuple (int_of_float x, int_of_float y)

  let cross a b =
    V.cross a b 

  let m_apply3 m v =
    M.apply3 m v

  let m_apply4 m v =
    M.apply4 m v

end

include MV

let pi = acos(-1.0)

let inv_pi = 1. /. pi

let eps_cosine  = 1e-6
let eps_ray     = 1e-3

let ifloor x = int_of_float @@ floor x

let sqr x = x *. x

let clamp a b v =
  min b (max a v)

(* sRGB luminance *)
let luminance aRGB =
  let x, y, z = tuple_of_v3 aRGB in
  0.212671 *. x +. 0.715160 *. y +. 0.072169 *. z


let v2i_of_v2f v =
  let x, y = V2f.to_tuple v in
  V2i.of_tuple (truncate @@ floor x, truncate @@ floor y)

let v3i_of_v v =
  let x,y,z = tuple_of_v3 v in
  V3i.of_tuple (int_of_float x, int_of_float y, int_of_float z)

let isZero v =
  let x, y, z = tuple_of_v3 v in
  x = 0. && y = 0. && z = 0.

let notZero v =
  not @@ isZero v

let pprintf_v4 out v =
  let x,y,z,w = V4.to_tuple v in
  Printf.fprintf out "{%.03e,%.03e,%.03e,%.03e}" x y z w

let pprintf_v2 out v =
  let x,y = V2f.to_tuple v in
    Printf.fprintf out "{%.03e,%.03e}" x y

let pprintf_v out v =
  let x,y,z = tuple_of_v3 v in
    Printf.fprintf out "{%.03e,%.03e,%.03e}" x y z

let pprinti_v2 out v =
  let x,y = V2i.to_tuple v in
    Printf.fprintf out "{%d,%d}" x y

let pprinti_v out v =
  let x,y,z = V3i.to_tuple v in
    Printf.fprintf out "{%d,%d,%d}" x y z

let pprintf_m out m =
  let pprintf out v =
    let x,y,z,w = V4.to_tuple v in
    Printf.fprintf out "| %.03e %.03e %.03e %.03e |" x y z w
  in
  Printf.fprintf out "%a\n%a\n%a\n%a"
                 pprintf (M.row m 0)
                 pprintf (M.row m 1)
                 pprintf (M.row m 2)
                 pprintf (M.row m 3)

let dump_v4 v =
  pprintf_v4 stderr v

let dump_m str m =
  let dump_col v =
    let x,y,z,w = V4.to_tuple v in
    Printf.fprintf stderr "| %.03e %.03e %.03e %.03e |\n" x y z w
  in
  Printf.fprintf stderr "%s\n" str;
  for i = 0 to 3 do
    dump_col (M.row m i)
  done


(* --------------------------------------------- *)

let fresnelDielectric aCosInc mIOR =
  if mIOR < 0.
  then 1.
  else
    let aCosInc, etaIncOverEtaTrans =
      if aCosInc < 0.
      then 
        (-.aCosInc), mIOR
      else
        aCosInc, 1. /. mIOR
    in
    let sinTrans2 = (sqr etaIncOverEtaTrans) *. (1. -. (sqr aCosInc)) in
    let  cosTrans = sqrt (max 0. (1. -. sinTrans2)) in

    let term1 = etaIncOverEtaTrans *. cosTrans in
    let  rParallel =
      (aCosInc -. term1) /. (aCosInc +. term1) in

    let term2 = etaIncOverEtaTrans *. aCosInc in
    let rPerpendicular =
      (term2 -. cosTrans) /. (term2 +. cosTrans) in

    0.5 *. ((sqr rParallel) +. (sqr rPerpendicular))


(* reflect vector through (0,0,1) *)
let reflectLocal aVector =
  let x, y, z = tuple_of_v3 aVector in
  v3_of_tuple (-.x, -.y, z)


(* //////////////////////////////////////////////////////////////////////// *)
(* Cosine lobe hemisphere sampling *)

let samplePowerCosHemisphereW aSamples aPower =
  let term1 = 2. *. pi *. (V2f.get aSamples 0) in
  let term2 = (V2f.get aSamples 1) ** (1. /. (aPower +. 1.)) in
  let term3 = sqrt (1. -. term2 *. term2) in
  let oPdfW = (aPower +. 1.) *. (term2 ** aPower) *. (0.5 *. inv_pi) in
  let sample = v3_of_tuple ((cos term1) *. term3, (sin term1) *. term3, term2) in
    oPdfW, sample

let powerCosHemispherePdfW aNormal aDirection aPower =
  let cosTheta = max 0. (V.dot aNormal aDirection) in
  (aPower +. 1.) *. (cosTheta ** aPower) *. (0.5 *. inv_pi)

(*////////////////////////////////////////////////////////////////////////
 * Disc sampling *)

let sampleConcentricDisc aSamples =
  let quarter_pi = pi /. 4. in
  let aSamplesX, aSamplesY = V2f.to_tuple aSamples in
    let a = 2. *. aSamplesX -. 1.0   (* (a,b) is now on [-1,1]^2 *)
    and b = 2. *. aSamplesY -. 1.0 in
    let r, phi =
      if a > -.b      (* region 1 or 2 *)
      then
        if a > b   (* region 1, also |a| > |b| *)
        then
          a, quarter_pi  *. (b /. a)
        else        (* region 2, also |b| > |a| *)
          b, quarter_pi *. (2. -. (a /. b))
      else            (* region 3 or 4 *)
        if a < b   (* region 3, also |a| >= |b|, a <> 0 *)
        then
          -.a, quarter_pi *. (4. +. (b /. a))
        else        (* region 4, |b| >= |a|, but a==0 and b==0 could occur. *)
          -.b, (
          if (b <> 0.)
          then quarter_pi *. (6. -. (a /. b))
          else 0.
        )
    in
    V2f.of_tuple (r *. (cos phi), r *. (sin phi))

let concentricDiscPdfA () = inv_pi

(*////////////////////////////////////////////////////////////////////////
 * Sample direction in the upper hemisphere with cosine-proportional pdf
 * The returned PDF is with respect to solid angle measure  *)

let sampleCosHemisphereW aSamples =
  let aSamplesX, aSamplesY = V2f.to_tuple aSamples in
  let term1 = 2. *. pi *. aSamplesX 
  and term2 = sqrt (1. -. aSamplesY) in
  let retZ = sqrt aSamplesY in
  let ret = v3_of_tuple (
                (cos term1) *. term2,
                (sin term1) *. term2,
                retZ)
  in retZ *. inv_pi, ret

let cosHemispherePdfW aNormal aDirection =
    (max 0. @@ V.dot aNormal aDirection) *. inv_pi

(* Sample Triangle
 * returns barycentric coordinates *)
let sampleUniformTriangle aSamples =
  let aSamplesX, aSamplesY = V2f.to_tuple aSamples in
  let term = sqrt aSamplesX in
  V2f.of_tuple (1. -. term, aSamplesY *. term)


(*////////////////////////////////////////////////////////////////////////
// Sphere sampling *)

let sampleUniformSphereW aSamples =
  let aSamplesX, aSamplesY = V2f.to_tuple aSamples in
  let term1 = 2. *. pi *. aSamplesX in
  let term2 = 2. *. (sqrt @@ aSamplesY -. aSamplesY *. aSamplesY) in
  let ret =
    v3_of_tuple (
        (cos term1) *. term2,
        (sin term1) *. term2,
        1. -. 2. *. aSamplesY)
  and oPdfSA = inv_pi *. 0.25 in
  oPdfSA, ret

let uniformSpherePdfW () =
  (* (1. /. (4. * pi)*)
  inv_pi *. 0.25

(*////////////////////////////////////////////////////////////////////////
// Utilities for converting PDF between Area (A) and Solid angle (W)
// WtoA = PdfW * cosine / distance_squared
// AtoW = PdfA * distance_squared / cosine *)

let pdfWtoA aPdfW aDist aCosThere =
  aPdfW *. (abs_float aCosThere) /. (sqr aDist)

let pdfAtoW aPdfA aDist aCosThere =
  aPdfA *. (sqr aDist) /. (abs_float aCosThere)

