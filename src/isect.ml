open Utils

type t = {
    dist    : float; (* !< Distance to closest intersection (serves as ray.tmax) *)
    matID   : int;   (* !< ID of intersected material *)
    lightID : int;   (* !< ID of intersected light (if < 0, then none) *)
    normal  : V.t;   (* !< Normal at the intersection *)
}

let default = {
  dist    = 1e36;
  matID   = 0;
  lightID = -1;
  normal  = V.null ();
}

let make dist matID lightID normal = 
  {dist; matID; lightID; normal}

let dist i = i.dist
let set_dist i dist = { i with dist }
let matID i = i.matID
let lightID i = i.lightID
let normal i = i.normal 

let dump ppf isect =
  Printf.fprintf ppf "{ dist = %.03e; matID = %d; lightID = %d; normal = %a }" isect.dist isect.matID isect.lightID pprintf_v isect.normal 
