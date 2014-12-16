open Utils

let unit0 = v3_of_tuple (1.,0.,0.)
let unit1 = v3_of_tuple (0.,1.,0.)
let unit2 = v3_of_tuple (0.,0.,1.)

(* actually a 3x3 matrix *)
type t = {
    mX : V.t;
    mY : V.t;
    mZ : V.t;
}

let identity () = {
  mX = unit0;
  mY = unit1;
  mZ = unit2;
}

let make mX mY mZ = {
  mX;
  mY;
  mZ;
}
let setFromZ z =

  let mZ = V.normalize z in
  let tmpX = if (abs_float @@ V.get mZ 0) > 0.99
             then unit1 else unit0 in
  let mY = V.normalize @@ cross mZ tmpX in
  let mX = cross mY mZ in {
    mX;
    mY;
    mZ
  }

let toWorld self a =
  let ax, ay, az = tuple_of_v3 a in
  V.add (V.scale self.mX ax)
  @@ V.add (V.scale self.mY ay)
           (V.scale self.mZ az)

let toLocal self a =
  v3_of_tuple (V.dot a self.mX,V.dot a self.mY,V.dot a self.mZ) 

let binormal self = self.mX
let tangent self = self.mY
let normal self = self.mZ
