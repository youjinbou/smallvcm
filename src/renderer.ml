open Utils

class virtual abstractRenderer aScene (minPathLength,maxPathLength) = 
object(self)

  val mMaxPathLength = maxPathLength
  val mMinPathLength = minPathLength

  val mutable mIterations    = 0
  val mutable mFramebuffer   = 
    Framebuffer.setup (Camera.resolution @@ Scene.camera aScene)

  val         mScene         = aScene

  method virtual runIteration : int -> unit

  method getFramebuffer =
    let oFramebuffer = Framebuffer.clone mFramebuffer in
    if mIterations > 0
    then Framebuffer.scale oFramebuffer (1. /. (float mIterations));
    oFramebuffer

  (*! Whether this renderer was used at all *)
  method wasUsed =  mIterations > 0

end

