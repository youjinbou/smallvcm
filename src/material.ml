open Utils

type t = {
  (* diffuse is simply added to the others *)
  mDiffuseReflectance : V.t;
  (* Phong is simply added to the others *)
  mPhongReflectance   : V.t;
  mPhongExponent      : float;

  (* mirror can be either simply added, or mixed using Fresnel term *)
  (* this is governed by mIOR, if it is >= 0, fresnel is used, otherwise *)
  (* it is not *)
  mMirrorReflectance  : V.t;

  (* When mIOR >= 0, we also transmit (just clear glass) *)
  mIOR                : float;
}

let diffuseReflectance m = m.mDiffuseReflectance
let phongReflectance   m = m.mPhongReflectance
let phongExponent      m = m.mPhongExponent
let mirrorReflectance  m = m.mMirrorReflectance
let ior                m = m.mIOR


let default = {
  mDiffuseReflectance = V.null ();
  mPhongReflectance   = V.null ();
  mPhongExponent      = 1.0;
  mMirrorReflectance  = V.null ();
  mIOR                = -1.0;
}

let make ?diffuse_refl ?phong_refl ?(phong_exp=1.0) ?mirror_refl ?(ior=(-1.0)) () =
  let nullop = function Some v -> v | None -> V.null () in {
    mDiffuseReflectance = nullop diffuse_refl;
    mPhongReflectance   = nullop phong_refl;
    mPhongExponent      = phong_exp;
    mMirrorReflectance  = nullop mirror_refl;
    mIOR                = ior;
  }

let dump out m = 
  Printf.fprintf out "{ diffuse = %a; phong = %a; exponent = %.03e; mirror = %a; ior = %.03e }" 
                 pprintf_v m.mDiffuseReflectance
                 pprintf_v m.mPhongReflectance
                 m.mPhongExponent
                 pprintf_v m.mMirrorReflectance
                 m.mIOR
