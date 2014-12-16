open Utils

type t = {
    org  : V.t;  (* !< Ray origin *)
    dir  : V.t;  (* !< Ray direction *)
    tmin : float; (* !< Minimal distance to intersection *)
}

let make org dir tmin = 
  (* Offset ray origin instead of setting tmin due to numeric
   * issues in ray-sphere intersection. The isect.dist has to be
   * extended by this EPS_RAY after hit point is determined *)
  {org = V.add org (V.scale dir eps_ray); dir; tmin}
(*    {org = org; dir; tmin = tmin +. eps_ray} *)
(*    {org; dir; tmin} *)

let origin r = r.org
let dir r = r.dir
let tmin r = r.tmin

let dump ppf ray =
  Printf.fprintf ppf "{ org = %a; dir = %a; tmin = %.03e }" pprintf_v ray.org pprintf_v ray.dir ray.tmin 
