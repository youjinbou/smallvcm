open Utils

module A = Array

type t = {
  mBBoxMin  : V.t;
  mBBoxMax  : V.t;
  mIndices  : int array;
  mCellEnds : int array;

  mRadius      : float;
  mRadiusSqr   : float;
  mCellSize    : float;
  mInvCellSize : float;
}

let bbMin h    = h.mBBoxMin  
let bbMax h    = h.mBBoxMax  
let indices h    = h.mIndices  
let cellEnds h   = h.mCellEnds 

let radius h       = h.mRadius      
let radiusSqr h    = h.mRadiusSqr   
let cellSize h     = h.mCellSize    
let invCellSize h  = h.mInvCellSize

let resize a n d =
  let l = A.length a in
  if n > l
  then let b = A.make n d in
       A.blit a 0 b 0 l;
       b
  else A.init n (fun i -> A.get a i)

let reserve h n =
  let a = resize h.mCellEnds n 0 in
  { h with mCellEnds = a }

let getCellRange self aCellIndex =
  if aCellIndex = 0 
  then V2i.of_tuple (0, A.get self.mCellEnds 0)
  else V2i.of_tuple (A.get self.mCellEnds (aCellIndex-1), A.get self.mCellEnds aCellIndex)

let getCellIndexFromCoord cellLength aCoord =
  let ( * ) a b = (a * b) land 0xffffffff in
  let x, y, z = V3i.to_tuple aCoord in
  let idx = ((x * 73856093) lxor (y * 19349663) lxor (z * 83492791)) mod cellLength in
(*  Printf.fprintf stderr "getCellIndexFromCoord : {%d,%d,%d} -> %d [%d]\n" x y z idx cellLength; *)
  if idx < 0 then idx + cellLength else idx
                           
let getCellIndexFromPoint mBBoxMin mInvCellSize cellLength aPoint =
  let distMin = V.sub aPoint mBBoxMin in
  let coordF = V.map floor (V.scale distMin mInvCellSize) in
  let coordI = v3i_of_v coordF in
  getCellIndexFromCoord cellLength coordI

module type PARTICLE = sig
    type t
    val getPosition : t -> V.t
end

module type QUERY = sig

    module Particle : PARTICLE

    type t
    val getPosition : t -> V.t
    val process : t -> Particle.t -> unit

end

module type S =
sig
  module Particle : PARTICLE
  type query

  val build : Particle.t array -> int -> float -> t
  val process : t -> Particle.t array -> query -> unit
end

module Make (Query : QUERY) : S with module Particle = Query.Particle
                                 and type query = Query.t = struct

  module Particle = Query.Particle

  type query = Query.t

  module C = struct 
    include Array
    let data x = x
  end

  let build aParticles aCellCount mRadius =
    let particleCount = A.length aParticles in
    let mRadiusSqr = sqr mRadius
    and mCellSize  = mRadius *. 2.0 in
    let mInvCellSize = 1. /. mCellSize
    and mBBoxMin = ref @@ V.make 1e36
    and mBBoxMax = ref @@ V.make (-1e36) in
    for i = 0 to pred particleCount do
      let pos = Particle.getPosition @@ A.get aParticles i in
      mBBoxMax := vmax !mBBoxMax pos;
      mBBoxMin := vmin !mBBoxMin pos;
    done;
    let mBBoxMax = !mBBoxMax
    and mBBoxMin = !mBBoxMin in
    let indices = Vector.make particleCount 0 in
    let cellEnds = C.make aCellCount 0 in 
    let () = Printf.fprintf stderr "Hashgrid.build : mBBoxMin = %a ; mBBoxMax = %a ; size = %d ; invCellSize = %e\n"
                   pprintf_v mBBoxMin
                   pprintf_v mBBoxMax
                   particleCount
                   mInvCellSize in
    (* set mCellEnds[x] to number of particles within x *)
    for i = 0 to pred particleCount do
      let pos = Particle.getPosition (A.get aParticles i) in
      let idx = getCellIndexFromPoint mBBoxMin mInvCellSize (C.length cellEnds) pos in
      let v = C.get cellEnds idx in
      C.set cellEnds idx (succ v);
    done;

    (* run exclusive prefix sum to really get the cell starts
     * mCellEnds[x] is now where the cell starts *)
    let _ = 
      C.fold_left
        (fun (i,sum) v -> 
         C.set cellEnds i sum;
         succ i, sum + v
        ) (0,0) cellEnds
    in
    for i = 0 to pred particleCount do
      let pos = Particle.getPosition (A.get aParticles i) in
      let idx = getCellIndexFromPoint mBBoxMin mInvCellSize (C.length cellEnds) pos in
(*      Printf.fprintf stderr "idx = %d\n" idx; *)
      if idx < 0 then (
        Printf.fprintf stderr "Hashgrid.build : idx = %d, pos = %a\n" idx pprintf_v pos;
        exit (-1)
      );
      let targetIdx = C.get cellEnds idx in
(*      Printf.fprintf stderr "targetIdx = %d\n" targetIdx;*)
      C.set cellEnds idx (succ targetIdx);
      if targetIdx < 0 then (
        Printf.fprintf stderr "Hashgrid.build : targetIdx = %d, idx = %d, pos = %a\n" targetIdx idx pprintf_v pos;
        exit (-1)
      );
      Vector.set indices targetIdx i
    done;
(*    let () = prerr_endline "Hashgrid.build done" in *)
    let self = {
      mBBoxMin;
      mBBoxMax;
      mIndices = Vector.data indices;
      mCellEnds = C.data cellEnds;
      mRadius;
      mRadiusSqr;
      mCellSize;
      mInvCellSize;
    } in
    (* now mCellEnds[x] points to the index right after the last
     * element of cell x *)
    (* DEBUG *)
    (* Printf.fprintf stderr "mCellEnds size = %d\n" @@ Array.length self.mCellEnds; *)
    (* for i = 0 to pred particleCount do *)
    (*   let pos = Particle.getPosition (A.get aParticles i) in *)
    (*   let range = getCellRange self @@ getCellIndexFromPoint mBBoxMin mInvCellSize (Array.length self.mCellEnds) pos in *)
    (*   let x,y = V2i.to_tuple range in *)
    (*   Printf.fprintf stderr "particle num : %d -> range %d %d\n" i x y; *)
    (*   let rec loop x y = *)
    (*     x < y && ( *)
    (*       (A.get self.mIndices x = i) *)
    (*      || loop (succ x) y) *)
    (*   in if not (loop x y) then Printf.fprintf stderr "Error at particle %d\n" i *)
    (* done; *)
    self

  let process self aParticles aQuery =
    let queryPos = Query.getPosition aQuery in
    let distMin = V.sub queryPos self.mBBoxMin
    and distMax = V.sub self.mBBoxMax queryPos in
    let b =
      V.get distMin 0 < 0. || V.get distMin 1 < 0. || V.get distMin 2 < 0. 
      || V.get distMax 0 < 0. || V.get distMax 1 < 0. || V.get distMax 2 < 0. 
    in
    if not b then (
      let f i = 
        let cellPt = V.get distMin i *. self.mInvCellSize in
        let coordF = floor cellPt  in
        let fractCoord = cellPt -. coordF
        and p = int_of_float coordF in
        p, p + (if fractCoord < 0.5 then -1 else 1)
      in
      let px, pxo = f 0
      and py, pyo = f 1
      and pz, pzo = f 2 in
      for j = 0 to 7 do
        let idx =
          match j with
          | 0 -> (px , py , pz )
          | 1 -> (px , py , pzo)
          | 2 -> (px , pyo, pz )
          | 3 -> (px , pyo, pzo)
          | 4 -> (pxo, py , pz )
          | 5 -> (pxo, py , pzo)
          | 6 -> (pxo, pyo, pz )
          | 7 -> (pxo, pyo, pzo)
          | _ -> assert false in
        let activeRange = getCellRange self @@ getCellIndexFromCoord (Array.length self.mCellEnds) (V3i.of_tuple idx) in
        let x, y = V2i.to_tuple activeRange in
        let rec loop x y =
          if x < y then (
            let particleIndex = A.get self.mIndices x in
            let particle = A.get aParticles particleIndex in
            let distSqr =
              lenSqr (V.sub (Query.getPosition aQuery) (Particle.getPosition particle)) in
            
            if distSqr <= self.mRadiusSqr
            then Query.process aQuery particle;
            loop (succ x) y
          )
        in loop x y
      done
    )

end
