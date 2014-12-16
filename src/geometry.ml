open Utils

module type T = sig
    type t
    val intersect : t -> Ray.t -> Isect.t -> Isect.t option
    val intersectP : t -> Ray.t -> Isect.t -> Isect.t option
    val growBB : t -> (V.t * V.t) -> (V.t * V.t)
end

module Triangle = struct

  type t = {
    p : V.t * V.t * V.t;
    matID : int;
    mNormal : V.t;
  }

  let make v1 v2 v3 i = {
    p = v1,v2,v3;
    matID = i;
    mNormal = V.normalize @@ cross (V.sub v2 v1) (V.sub v3 v1)
  }

  let intersect triangle aRay oResult =
    let p1,p2,p3 = triangle.p in
    let ao = V.sub p1 (Ray.origin aRay)
    and bo = V.sub p2 (Ray.origin aRay)
    and co = V.sub p3 (Ray.origin aRay) in
    let v0 = cross co bo 
    and v1 = cross bo ao
    and v2 = cross ao co in
    let v0d = V.dot v0 (Ray.dir aRay)
    and v1d = V.dot v1 (Ray.dir aRay)
    and v2d = V.dot v2 (Ray.dir aRay) in
    if (v0d < 0. && v1d < 0. && v2d < 0.)
       || (v0d >= 0. && v1d >= 0. && v2d >= 0.)
    then
      let distance = (V.dot triangle.mNormal ao) /. (V.dot triangle.mNormal (Ray.dir aRay)) in
      if distance > (Ray.tmin aRay) && distance < (Isect.dist oResult)
      then Some (Isect.make distance triangle.matID (Isect.lightID oResult) triangle.mNormal)
      else None
    else None

  let intersectP = intersect

  let growBB triangle (aoBBoxMin,aoBBoxMax) =
    (* Printf.fprintf stderr "Triangle::growBBox(%d) : [%a,%a]" triangle.matID pprintf_v aoBBoxMin pprintf_v aoBBoxMax; *)
    let p1, p2, p3 = triangle.p in
    let bmin, bmax =
      vmin p1 @@ vmin p2 @@ vmin p3 aoBBoxMin,
      vmax p1 @@ vmax p2 @@ vmax p3 aoBBoxMax
    in
(*    Printf.fprintf stderr " => [%a,%a]\n" pprintf_v bmin pprintf_v bmax;
    Printf.fprintf stderr "< %a , %a , %a >\n" pprintf_v p1 pprintf_v p2 pprintf_v p3; *)
    bmin,bmax

  let dump {p = (p1,p2,p3);_} =
    Printf.fprintf stderr "< %a , %a , %a >\n" pprintf_v p1 pprintf_v p2 pprintf_v p3

end

module Sphere = struct

  type t = {
    center : V.t;
    radius : float;
    matID  : int;
  }

  let make center radius matID = {center; radius; matID}

  let intersect sphere aRay oResult =
    (* we transform ray origin into object space (center == origin) *)
    let transformedOrigin = V.sub (Ray.origin aRay) sphere.center in
    let a = V.dot (Ray.dir aRay) (Ray.dir aRay)
    and b = 2. *. V.dot (Ray.dir aRay) transformedOrigin
    and c = (V.dot transformedOrigin transformedOrigin) -. (sphere.radius *. sphere.radius) in
    (* Must use doubles, because when B ~ sqrt(B*B - 4*A*C)
         the resulting t is imprecise enough to get around ray epsilons *)
    let disc = b*.b -. 4.*.a*.c in
    if disc < 0.
    then None
    else 
      let discSqrt = sqrt disc in
      let q = if b < 0. then (-.b -. discSqrt) /. 2. else (-.b +. discSqrt) /. 2. in

      let t0 = q /. a
      and t1 = c /. q in
      let t0, t1 =
        if t0 > t1 then t1, t0 else t0, t1 in
      let res k = 
        Isect.make k sphere.matID (Isect.lightID oResult)
                   (V.normalize @@
                      V.add transformedOrigin 
                            (V.scale (Ray.dir aRay) k))
      in
      if t0 > Ray.tmin aRay && t0 < Isect.dist oResult
      then Some (res t0)
      else if t1 > Ray.tmin aRay && t1 < Isect.dist oResult
      then Some (res t1)
      else None

  let intersectP = intersect

  let growBB sphere (aoBBoxMin,aoBBoxMax) =
    (* Printf.fprintf stderr "Sphere::growBBox(%d) : [%a,%a]" sphere.matID pprintf_v aoBBoxMin pprintf_v aoBBoxMax; *)
    let p = sphere.center
    and half = V.make sphere.radius in
    let get_half v i =
      let hlf j = if i land j != 0 then -. (V.get v j) else (V.get v j) in
      v3_of_tuple (hlf 0, hlf 1, hlf 2)
    in
    let rec check i bmin bmax =
      if i >= 8 
      then (
(*        Printf.fprintf stderr " => [%a,%a]\n" pprintf_v bmin pprintf_v bmax; *)
        bmin,bmax
      )
      else 
        let h = get_half half i in
        let p = V.add p h in
        check (succ i) (vmin bmin p) (vmax bmax p)
    in check 0 aoBBoxMin aoBBoxMax

end

type t = Triangle of Triangle.t | Sphere of Sphere.t | List of t list

let rec intersect self ray result =
  match self with
  | Sphere s -> Sphere.intersect s ray result
  | Triangle t -> Triangle.intersect t ray result
  | List l -> intersectList l ray result
and intersectList l ray result =
  let fold acc x =
    match acc with 
    | None -> (
      match intersect x ray result with
      | None -> None
      | r    -> r)
    | Some r ->  (
      match intersect x ray r with
      | None -> Some r
      | r'    -> r')
  in List.fold_left fold None l

let rec intersectP self ray result =
  match self with
  | Sphere s -> Sphere.intersectP s ray result
  | Triangle t -> Triangle.intersectP t ray result
  | List l -> intersectListP l ray result
and intersectListP l ray result =
  let fold acc x =
    match acc with 
    | None -> (
      match intersect x ray result with
      | None -> None
      | r    -> r)
    | r -> r
  in List.fold_left fold None l

let rec growBB x bb =
  match x with
  | Sphere s -> Sphere.growBB s bb
  | Triangle t -> Triangle.growBB t bb
  | List l -> growBBList l bb
and growBBList l bb =
  List.fold_left (fun bb x -> growBB x bb) bb l
