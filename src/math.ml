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

(* let pi = 3.14159265358979 *)
let pi = acos (-1.0)
let inv_pi = 1. /. pi


(*////////////////////////////////////////////////////////////////////////
 * Math section *)

module type SCALAR = sig

    type t
    val zero : t
    val opp : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val inv : t -> t
    val sqrt : t -> t
end

module Float : SCALAR with type t = float = struct
  type t = float
  let zero = 0.
  let opp = ( ~-. )
  let add = ( +. )
  let sub = ( -. )
  let mul = ( *. )
  let div = ( /. )
  let inv x = (1. /. x)
  let sqrt = sqrt
end

module Int : SCALAR with type t = int = struct
  type t = int
  let zero = 0
  let opp = ( ~- )
  let add = ( + )
  let sub = ( - )
  let mul = ( * )
  let div = ( / )
  let inv x = truncate (1. /. float x)
  let sqrt x = truncate (sqrt @@ float x)
end

module type V = sig

    type scalar
    type t
    type tuple
    val null : unit -> t
    val make : scalar -> t
    val of_tuple : tuple -> t
    val to_tuple : t -> tuple
    val get : t -> int -> scalar
    val set : t -> int -> scalar -> t
    val map : (scalar -> scalar) -> t -> t
    val opp : t -> t
    val map2 : (scalar -> scalar -> scalar) -> t -> t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val dot : t -> t -> scalar
    val scale : t -> scalar -> t
    val fold_left : ('a -> scalar -> 'a) -> 'a -> t -> 'a
    val fold_right : (scalar -> 'a -> 'a) -> t -> 'a -> 'a

end

module V2x (S : SCALAR) : V with type scalar = S.t and type tuple = S.t * S.t = struct

  type scalar = S.t
  type t = scalar * scalar
  type tuple = t

  let zero           = S.zero, S.zero
  let null ()    : t = zero
  let make x     : t = x, x
  external of_tuple : scalar * scalar -> t = "%identity"
  external to_tuple : t -> scalar * scalar = "%identity"

  let get (x,y) : int -> scalar = function 0 -> x | 1 -> y | _ -> failwith "invalid index"
  let set (x,y) i v = match i with
      0 -> (v,y)
    | 1 -> (x,v)
    | _ -> failwith "invalid index"

  (* unary minus *)
  let map f (x,y) = f x, f y
  let opp = map S.opp 

  (* binary operations *)
  let map2 op (x1,y1) (x2,y2) = op x1 x2, op y1 y2
  let add = map2 S.add
  let sub = map2 S.sub
  let mul = map2 S.mul
  let div = map2 S.div

  let dot (x1,y1) (x2,y2) = S.add (S.mul x1 x2) (S.mul y1 y2)

  let scale v x = mul v (make x)

  let fold_left f a (x,y) = f (f a x) y
  let fold_right f (x,y) a = f x (f y a)
                      
end

module V2f = V2x(Float)
module V2i = V2x(Int)

module type V3 = sig

    type scalar
    type t
    type tuple = scalar * scalar * scalar
    val null : unit -> t
    val make : scalar -> t
    val of_tuple : tuple -> t
    val to_tuple : t -> tuple
    val get : t -> int -> scalar
    val set : t -> int -> scalar -> t
    val map : (scalar -> scalar) -> t -> t
    val opp : t -> t
    val map2 : (scalar -> scalar -> scalar) -> t -> t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val dot : t -> t -> scalar
    val scale : t -> scalar -> t

    val fold_left : ('a -> scalar -> 'a) -> 'a -> t -> 'a
    val fold_right : (scalar -> 'a -> 'a) -> t -> 'a -> 'a

    module V2 : V with type scalar = scalar and type tuple = scalar * scalar

    val getXY : t -> V2.t
    val ofXY  : V2.t -> scalar -> t

    val max : t -> scalar
    val isZero : t -> bool
end

module V3x (S : SCALAR) : V3 with type scalar = S.t and type tuple = S.t * S.t * S.t = struct

  type scalar = S.t
  type t = scalar * scalar * scalar
  type tuple = t

  module V2 : V with type scalar = scalar and type tuple = scalar * scalar = V2x(S)

  let zero       : t = S.zero, S.zero, S.zero
  let null ()    : t = zero
  let make x     : t = x, x, x
  external of_tuple : scalar * scalar * scalar -> t = "%identity"
  external to_tuple : t -> scalar * scalar * scalar = "%identity"

  let get (x,y,z) : int -> scalar = function 0 -> x | 1 -> y | 2 -> z | _ -> failwith "invalid index"
  let set (x,y,z) i v = match i with
      0 -> (v,y,z)
    | 1 -> (x,v,z)
    | 2 -> (x,y,v)
    | _ -> failwith "invalid index"

  let getXY v = let x,y,z = to_tuple v in V2.of_tuple (x,y)
  let ofXY v c = let x,y = V2.to_tuple v in (x,y,c)
  let max ((x,y,z) : t) = max x (max y z)

  let isZero (x,y,z) = not (x <> S.zero || y <> S.zero || z <> S.zero)

  (* unary minus *)
  let map op (x1,y1,z1) = op x1, op y1, op z1
  let opp = map S.opp

  (* binary operations *)
  let map2 op (x1,y1,z1) (x2,y2,z2) = op x1 x2, op y1 y2, op z1 z2
  let add = map2 S.add
  let sub = map2 S.sub
  let mul = map2 S.mul
  let div = map2 S.div

  let dot (x1,y1,z1) (x2,y2,z2) = S.add (S.mul x1 x2) @@ S.add (S.mul y1 y2) (S.mul z1 z2)

  let scale v x = mul v (make x)

  let fold_left f a (x,y,z) = f (f (f a x) y) z
  let fold_right f (x,y,z) a = f x (f y (f z a)) 

end

module V3i = V3x(Int)

module V3f = struct

  include V3x(Float)

  let clone v = v

  let lenSqr (v : t) = dot v v
  let length (v : t) = sqrt @@ lenSqr v
                                          
  let cross v1 v2 =
    let x1,y1,z1 = to_tuple v1
    and x2,y2,z2 = to_tuple v2 in
    of_tuple (
        (y1 *. z2) -. ( z1 *. y2),
        (z1 *. x2) -. ( x1 *. z2),
        (x1 *. y2) -. ( y1 *. x2)
      )

  let normalize v =
    let len = length v in
    scale v (1. /. len)

end

module V4x(S : SCALAR) : V with type scalar = S.t 
                            and type tuple = S.t * S.t * S.t * S.t = struct
                                             
  type scalar = S.t
  type t = scalar * scalar * scalar * scalar
  type tuple = t

  module V3 : V3 with type scalar = scalar = V3x(S)

  let zero       : t = S.zero, S.zero, S.zero, S.zero
  let null ()    : t = zero
  let make x     : t = x, x, x, x
  external of_tuple : scalar * scalar * scalar * scalar -> t = "%identity"
  external to_tuple : t -> scalar * scalar * scalar * scalar = "%identity"

  let get (x,y,z,w) : int -> scalar = function 0 -> x | 1 -> y | 2 -> z | 3 -> w | _ -> failwith "invalid index"
  let set (x,y,z,w) i v = match i with
      0 -> (v,y,z,w)
    | 1 -> (x,v,z,w)
    | 2 -> (x,y,v,w)
    | 3 -> (x,y,z,v)
    | _ -> failwith "invalid index"


  let getXYZ (x,y,z,w) : V3.t = V3.of_tuple (x,y,z)
  let ofXYZ v3 = let x,y,z = V3.to_tuple v3 in (x,y,z,1.)
  let max (x,y,z) = max x (max y z)

  let isZero (x,y,z,w) = not (x <> S.zero || y <> S.zero || z <> S.zero || w <> S.zero)

  (* unary minus *)
  let map op (x1,y1,z1,w1) = op x1, op y1, op z1, op w1
  let opp = map S.opp

  (* binary operations *)
  let map2 op (x1,y1,z1,w1) (x2,y2,z2,w2) = op x1 x2, op y1 y2, op z1 z2, op w1 w2
  let add = map2 S.add
  let sub = map2 S.sub
  let mul = map2 S.mul
  let div = map2 S.div

  let dot (x1,y1,z1,w1) (x2,y2,z2,w2) = S.add (S.mul x1 x2) @@ S.add (S.mul y1 y2) @@ S.add (S.mul z1 z2) (S.mul w1 w2)

  let scale v x = mul v (make x)

  let fold_left f a (x,y,z,w) = f (f (f (f a x) y) z) w
  let fold_right f (x,y,z,w) a = f x (f y (f z (f w a)))

end

module V4f = V4x(Float)

let v3_of_v4 v4 =
  let x,y,z,w = V4f.to_tuple v4 in V3f.of_tuple (x,y,z)

let v4_of_v3 v3 = 
  let x,y,z = V3f.to_tuple v3 in V4f.of_tuple (x,y,z,1.)

module M = struct

  module V4 = struct
    include V4f
    let zero = null ()
    let sqrt v = failwith "not implemented"
    let inv v  = failwith "not implemented"
  end

  type vector = V4.t

  module Impl : V with type scalar = vector
                   and type tuple = vector * vector * vector * vector = V4x(V4)

  type t = Impl.t ref

  let null () = ref (Impl.null ())
  let make x = let v = V4.make x in ref (Impl.make v)

  let get m i j =
    V4.get (Impl.get !m j) i

  let set m i j v =
    m := Impl.set !m j (V4.set(Impl.get !m j) i v)

  let row m r =
    V4.of_tuple (get m r 0, get m r 1, get m r 2, get m r 3)

  let col m c =
    Impl.get !m c

  let setrow m r v =
    let a,b,c,d = V4.to_tuple v in
    set m r 0 a;
    set m r 1 b;
    set m r 2 c;
    set m r 3 d

  let setcol m c v =
    m := Impl.set !m c v

  let transposed m =
    ref (Impl.of_tuple (row m 0, row m 1, row m 2, row m 3))
           
  let add m1 m2 = ref (Impl.add !m1 !m2)
  let sub m1 m2 = ref (Impl.sub !m1 !m2)

  let mul m1 m2 = 
    let m = null () in 
    let mt = transposed m1 in
   for i = 0 to 3 do
      for j = 0 to 3 do
        set m i j (V4.dot (col mt i) (col m2 j))
      done
    done;
(*
    for r = 0 to 3 do
      for c = 0 to 3 do
        for i = 0 to 3 do
          set m r c ((get m r c) +. (get m1 r i) *. (get m2 i c));
        done
      done
    done;
 *)
    m

  let apply4 m v =
    let comp i = 
      V4.dot (row m i) v in
    V4.of_tuple (comp 0, comp 1, comp 2, comp 3)

  let apply3 m v =
    let v = 
      let x,y,z = V3f.to_tuple v in
      V4.of_tuple (x,y,z,1.) in
    let comp i = V4.dot (row m i) v in
    let w = comp 3 in
    let invW = 1. /. w in
    let r0 = invW *. (comp 0)
    and r1 = invW *. (comp 1)
    and r2 = invW *. (comp 2) in
    V3f.of_tuple (r0,r1,r2)

  let scaling x y z =
    let m = null () in
    set m 0 0 x;
    set m 1 1 y;
    set m 2 2 z;
    set m 3 3 1.;
    m

  let identity () =
    scaling 1. 1. 1.

  let scale m x =
    m := Impl.map (fun v -> V4.scale v x) !m

  let translation x y z =
    let res = identity () in
    let v = V4.of_tuple (x,y,z,1.) in
    setcol res 3 v;
    res

  let perspective aFov aFov aNear aFar =
    (* Camera points towards -z.  0 < near < far. *)
    (* Matrix maps z range [-near, -far] to [-1, 1], after homogeneous division. *)
    let f = 1. /. (tan (aFov *. pi /. 360.0))
    and d = 1. /. (aNear -. aFar) in
    let m00 = f
    and m11 = ~-.f
    and m22 = (aNear +. aFar) *. d
    and m32 = -1.
    and m23 = 2. *. aNear *. aFar *. d
    in
    ref (Impl.of_tuple (
             V4.of_tuple (m00, 0., 0., 0.),
             V4.of_tuple (0., m11, 0., 0.),
             V4.of_tuple (0., 0., m22, m32),
             V4.of_tuple (0., 0., m23, 0.)))

  let inverse m =
    let c1,c2,c3,c4    = Impl.to_tuple !m in
    let m0,m1,m2,m3     = V4.to_tuple c1
    and m4,m5,m6,m7     = V4.to_tuple c2
    and m8,m9,m10,m11   = V4.to_tuple c3
    and m12,m13,m14,m15 = V4.to_tuple c4 in

    let inv0 =
      m5 *. m10 *. m15 -.
        m5  *. m11 *. m14 -.
        m9  *. m6  *. m15 +.
        m9  *. m7  *. m14 +.
        m13 *. m6  *. m11 -.
        m13 *. m7  *. m10

    and inv4 =
      ~-.m4  *. m10 *. m15 +.
        m4  *. m11 *. m14 +.
        m8  *. m6  *. m15 -.
        m8  *. m7  *. m14 -.
        m12 *. m6  *. m11 +.
        m12 *. m7  *. m10

    and inv8 =
      m4  *. m9 *. m15 -.
        m4  *. m11 *. m13 -.
        m8  *. m5 *. m15 +.
        m8  *. m7 *. m13 +.
        m12 *. m5 *. m11 -.
        m12 *. m7 *. m9

    and inv12 =
      ~-.m4  *. m9 *. m14 +.
        m4  *. m10 *. m13 +.
        m8  *. m5 *. m14 -.
        m8  *. m6 *. m13 -.
        m12 *. m5 *. m10 +.
        m12 *. m6 *. m9

    and inv1 =
      ~-.m1  *. m10 *. m15 +.
        m1  *. m11 *. m14 +.
        m9  *. m2 *. m15 -.
        m9  *. m3 *. m14 -.
        m13 *. m2 *. m11 +.
        m13 *. m3 *. m10

    and inv5 =
      m0  *. m10 *. m15 -.
        m0  *. m11 *. m14 -.
        m8  *. m2 *. m15 +.
        m8  *. m3 *. m14 +.
        m12 *. m2 *. m11 -.
        m12 *. m3 *. m10

    and inv9 =
      -.m0  *. m9 *. m15 +.
        m0  *. m11 *. m13 +.
        m8  *. m1 *. m15 -.
        m8  *. m3 *. m13 -.
        m12 *. m1 *. m11 +.
        m12 *. m3 *. m9

    and inv13 =
      m0  *. m9 *. m14 -.
        m0  *. m10 *. m13 -.
        m8  *. m1 *. m14 +.
        m8  *. m2 *. m13 +.
        m12 *. m1 *. m10 -.
        m12 *. m2 *. m9

    and inv2 =
      m1  *. m6 *. m15 -.
        m1  *. m7 *. m14 -.
        m5  *. m2 *. m15 +.
        m5  *. m3 *. m14 +.
        m13 *. m2 *. m7 -.
        m13 *. m3 *. m6

    and inv6 =
      -.m0  *. m6 *. m15 +.
        m0  *. m7 *. m14 +.
        m4  *. m2 *. m15 -.
        m4  *. m3 *. m14 -.
        m12 *. m2 *. m7 +.
        m12 *. m3 *. m6

    and inv10 =
      m0  *. m5 *. m15 -.
        m0  *. m7 *. m13 -.
        m4  *. m1 *. m15 +.
        m4  *. m3 *. m13 +.
        m12 *. m1 *. m7 -.
        m12 *. m3 *. m5

    and inv14 =
      -.m0  *. m5 *. m14 +.
        m0  *. m6 *. m13 +.
        m4  *. m1 *. m14 -.
        m4  *. m2 *. m13 -.
        m12 *. m1 *. m6 +.
        m12 *. m2 *. m5

    and inv3 =
      -.m1 *. m6 *. m11 +.
        m1 *. m7 *. m10 +.
        m5 *. m2 *. m11 -.
        m5 *. m3 *. m10 -.
        m9 *. m2 *. m7 +.
        m9 *. m3 *. m6

    and inv7 =
      m0 *. m6 *. m11 -.
        m0 *. m7 *. m10 -.
        m4 *. m2 *. m11 +.
        m4 *. m3 *. m10 +.
        m8 *. m2 *. m7 -.
        m8 *. m3 *. m6

    and inv11 =
      -.m0 *. m5 *. m11 +.
        m0 *. m7 *. m9 +.
        m4 *. m1 *. m11 -.
        m4 *. m3 *. m9 -.
        m8 *. m1 *. m7 +.
        m8 *. m3 *. m5

    and inv15 =
      m0 *. m5 *. m10 -.
        m0 *. m6 *. m9 -.
        m4 *. m1 *. m10 +.
        m4 *. m2 *. m9 +.
        m8 *. m1 *. m6 -.
        m8 *. m2 *. m5
    in

    let det = m0 *. inv0 +. m1 *. inv4 +. m2 *. inv8 +. m3 *. inv12 in

    if (det = 0.)
    then identity()
    else let det = 1. /. det in
         ref (Impl.of_tuple (
                  V4.of_tuple (inv0 *. det, inv1 *. det, inv2 *. det, inv3 *. det),
                  V4.of_tuple (inv4 *. det, inv5 *. det, inv6 *. det, inv7 *. det),
                  V4.of_tuple (inv8 *. det, inv9 *. det, inv10 *. det, inv11 *. det),
                  V4.of_tuple (inv12 *. det, inv13 *. det, inv14 *. det, inv15 *. det)
                ))

end
