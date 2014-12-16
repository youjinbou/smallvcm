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

open Utils

module Make (Rng : Rng.S) = struct

  class eyelight aScene pathLengths aSeed =
  object(self)

    inherit Renderer.abstractRenderer aScene pathLengths 

    val mRng = Rng.make aSeed 

    method runIteration : int -> unit = fun aIteration ->
      let resX, resY = V2i.to_tuple @@ Camera.resolution @@ Scene.camera aScene in

      for pixID = 0 to pred @@ resX * resY do
        (*////////////////////////////////////////////////////////////////////////
         * Generate ray *)
        let x = pixID mod resX 
        and y = pixID / resX
        in
        let sample =
          V2f.add (V2f.of_tuple (float x, float y))
                  (if aIteration = 1 then V2f.make (0.5) else Rng.getVec2f mRng) in
        (* Printf.fprintf stderr "sample : {%f,%f}\n" (V2f.get sample 0) (V2f.get sample 1); *)
        let ray = Camera.generateRay (Scene.camera mScene) sample in
        let isect = Isect.default in
        match Scene.intersect mScene ray isect with
        | Some isect -> (
          let dotLN = -. (V.dot (Isect.normal isect) (Ray.dir ray)) in
          let v = if dotLN > 0. then V.make dotLN else v3_of_tuple (-.dotLN, 0., 0.) in
          Framebuffer.addColor mFramebuffer (v2i_of_v2f sample) v
        )
        | _ -> ()
      (*
        let c = V.make 1. in
        let r,g,b = tuple_of_v3 c in
        Printf.printf "{%f,%f,%f}" r g b;
        Framebuffer.addColor mFramebuffer (v2i_of_v2f sample) c;
       *)
      done;
      mIterations <- succ mIterations
                          
  end

  let renderer aScene pathLengths aSeed =
    (new eyelight aScene pathLengths aSeed :> Renderer.abstractRenderer)

end
