open Utils

module type S = sig

    type t

    val make : int -> t

    val getInt : t -> int

    val getInt32: t -> int32

    val getFloat : t -> float
    val getVec2f : t -> V2f.t
    val getVec3f : t -> V.t

end 

module Dummy : S = struct

  type t = unit

  let make _ = ()

  let getInt () = 0
  let getInt32 () = 0l

  let getFloat () = 0.
  let getVec2f () = V2f.null ()
  let getVec3f () = V.null ()

end

module Ocaml : S = struct

  type t = Random.State.t

  let make x =
    let () = Random.init x in
    Random.get_state ()

  let getInt s = 
     Random.State.int s max_int

  let getInt32 s =
     Random.State.int32 s Int32.max_int

  let getFloat s =
    Random.State.float s 1.0

  let getVec2f s = 
    let x = getFloat s
    and y = getFloat s in
    V2f.of_tuple (x,y)
                                    
  let getVec3f s =
    let x = getFloat s
    and y = getFloat s
    and z = getFloat s in
    v3_of_tuple (x,y,z)
 
end

module MersenneTwister : S = struct

  open Gmaths

  type t = MersenneTwister.State.t

  let make x =
    MersenneTwister.State.make (Int32.of_int x)

  let getInt s = MersenneTwister.Rng.int s max_int

  let getInt32 s = MersenneTwister.Rng.int32 s Int32.max_int (* actually not used in code *)

  let getFloat s = MersenneTwister.Rng.rawdouble s

  let getVec2f s = 
    let x = getFloat s
    and y = getFloat s in
    V2f.of_tuple (x,y)
                                    
  let getVec3f s =
    let x = getFloat s
    and y = getFloat s
    and z = getFloat s in
    v3_of_tuple (x,y,z)

end


module Legacy : S = struct

  module TeaImpl = struct

    type t = {
              mRound  : int;
      mutable mState0 : int32;
      mutable mState1 : int32;
    }
    
    let make aSeed0 aSeed1 = {
      mRound = 6;
      mState0 = aSeed0;
      mState1 = aSeed1;
    }

    let c1 = 0xa341316cl
    let c2 = 0xc8013ea4l
    let c3 = 0xad90777dl
    let c4 = 0x7e95761el

    let (+) = Int32.add
    and (<%<) = Int32.shift_left
    and (>%>) = Int32.shift_right_logical
    and (^) = Int32.logxor

    let update_state0 sum mState0 mState1 =
      mState0 + (((mState1 <%< 4) + c1) ^ (mState1+sum) ^ ((mState1>%>5)+c2))

    let update_state1 sum mState0 mState1 =
      mState1 + (((mState0<%<4)+c3) ^ (mState0+sum) ^ ((mState0>%>5)+c4))

    let getImpl self =
      let sum = ref 0l
      and delta = 0x9e3779b9l in
      for i = 0 to pred self.mRound do 
        sum := Int32.add !sum delta;
        self.mState0 <- update_state0 !sum self.mState0 self.mState1;
        self.mState1 <- update_state1 !sum self.mState0 self.mState1;
      done;
      self.mState0

  end

  type t = TeaImpl.t

  let make aSeed =
    TeaImpl.make (Int32.of_int aSeed) 5678l

  let getInt s = Int32.to_int @@ TeaImpl.getImpl s

  let getInt32 s = TeaImpl.getImpl s

  let getFloat s =
    let m = Int32.to_float Int32.max_int in
    let i = Int32.to_float @@ getInt32 s in
    mod_float ((i +. m +. 1.) *. (1.0 /. 4294967297.0) +. 0.5) 1.0


  let getVec2f s = 
    let x = getFloat s
    and y = getFloat s in
    V2f.of_tuple (x,y)
                 
  let getVec3f s =
    let x = getFloat s
    and y = getFloat s
    and z = getFloat s in
    v3_of_tuple (x,y,z)
                
end


type t =
  | Dummy
  | MersenneTwister
  | Legacy
  | Ocaml

let each =
  [ Dummy ; MersenneTwister; Legacy ; Ocaml]

let name = function
  | Dummy           -> "Dummy"
  | MersenneTwister -> "Mersenne Twister"
  | Legacy          -> "Legacy"
  | Ocaml           -> "Ocaml"

let acronym = function
  | Dummy           -> "dm"
  | MersenneTwister -> "mt"
  | Legacy          -> "lg"
  | Ocaml           -> "oc"

let of_acronym = function
  | "dm" -> Dummy
  | "mt" -> MersenneTwister
  | "lg" -> Legacy
  | "oc" -> Ocaml
  | _    -> failwith "invalid random number generator"

    
let default = MersenneTwister

let getModule = function
    Dummy           -> (module Dummy : S)
  | MersenneTwister -> (module MersenneTwister : S)
  | Legacy          -> (module Legacy : S)
  | Ocaml           -> (module Ocaml  : S)
