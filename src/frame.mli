type t = { mX : Utils.V.t; mY : Utils.V.t; mZ : Utils.V.t; }
val identity : unit -> t
val make : Utils.V.t -> Utils.V.t -> Utils.V.t -> t
val setFromZ : Utils.V.t -> t
val toWorld : t -> Utils.V.t -> Utils.V.t
val toLocal : t -> Utils.V.t -> Utils.V.t
val binormal : t -> Utils.V.t
val tangent : t -> Utils.V.t
val normal : t -> Utils.V.t
