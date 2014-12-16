open Utils

type t = {
  mColor : V.t array;
  mResolution : V2i.t;
  mResX : int;
  mResY : int;
}

let getPixel self aSample =
  let inRange k =
    let c = V2i.get aSample k
    and r = V2i.get self.mResolution k in
    c < 0 || c >= r in
  if inRange 0 && inRange 1
  then                      
    let x, y = V2i.to_tuple aSample in
    self.mColor.(x + y * self.mResX)
  else
    failwith "sample out of range"

(*////////////////////////////////////////////////////////////////////////
 * Accumulation *)

let addColor self aSample aColor =
  let inRange k =
    let c = V2i.get aSample k
    and r = V2i.get self.mResolution k in
    c >= 0 || c < r in
  let x, y = V2i.to_tuple aSample in
  if inRange 0 && inRange 1
  then 
    let idx = x + y * self.mResX in
    let c = self.mColor.(idx) in
(*    if notZero c then Printf.fprintf stderr "color already set at {%d,%d} (%d)\n" x y idx; *)
    self.mColor.(idx) <- V.add c aColor
  else Printf.fprintf stderr "sample out of range : {%i,%i}" x y

(*////////////////////////////////////////////////////////////////////////
 * Methods for framebuffer operations *)
let clone self = {
  self with
  mColor      = Array.init (Array.length self.mColor) (fun i -> V.clone self.mColor.(i));
}

let update_all f a =
  for i = 0 to pred (Array.length a) do
    a.(i) <- f i a.(i)
  done

let clear self =
  update_all (fun _ _ -> V.null ()) self.mColor

let setup aResolution =
  let mResX = V2i.get aResolution 0
  and mResY = V2i.get aResolution 1 in
  let self = {
    mResolution = aResolution;
    mResX;
    mResY;
    mColor = Array.init (mResX * mResY) (fun _ -> V.null ());
  } in
  clear self; self

let add self aOther =
  if Array.length aOther.mColor = Array.length self.mColor
  then update_all (fun i v -> V.add v aOther.mColor.(i)) self.mColor

let scale self aScale =
  update_all (fun i v -> V.scale v aScale) self.mColor

(*////////////////////////////////////////////////////////////////////////
 * Statistics *)
let totalLuminance self =
  Array.fold_left (fun l v -> l +. luminance v) 0. self.mColor

(*////////////////////////////////////////////////////////////////////////
 * Saving *)

module B = Binary

let savePPM self ?(aGamma=1.) aFilename =
  let open Printf in
  let invGamma = 1. /. aGamma in
  let ppm = open_out aFilename in
  fprintf ppm  "P3\n";
  fprintf ppm "%d %d\n" self.mResX self.mResY;
  fprintf ppm "255\n";
  for y = 0 to pred self.mResY do
    for x = 0 to pred self.mResX do
      let r,g,b = tuple_of_v3 self.mColor.(x + y * self.mResX) in
      let conv x = clamp 0 255 (int_of_float @@ ( x ** invGamma) *. 255.) in
      let r = conv r
      and g = conv g
      and b = conv b in
      fprintf ppm "%d %d %d\n" r g b;
    done
  done;
  fprintf ppm "\n";
  close_out ppm

let savePFM self aFilename =
  let open Printf in 
  let ppm = open_out aFilename in
  set_binary_mode_out ppm true;
  fprintf ppm "PF\n";
  fprintf ppm "%d %d\n" self.mResX self.mResY;
  fprintf ppm "255\n";
  let b = B.make 8 in
  for y = 0 to pred self.mResY do
    for x = 0 to pred self.mResX do
      let write_component x =
        B.set_binstring_of_float x b;
        B.output_bytes ppm b
      in
      V.fold_left (fun () x -> write_component x) () self.mColor.(x + y * self.mResX)
    done
  done;
  close_out ppm

(*////////////////////////////////////////////////////////////////////////
 * Saving BMP *)

type uint32 = int32
type int16 = int

type bmpHeader = {
  mFileSize   : uint32;      (* Size of file in bytes *)
  mReserved01 : uint32;      (* 2x 2 reserved bytes *)
  mDataOffset : uint32;      (* Offset in bytes where data can be found (54) *)

  mHeaderSize : uint32;      (* 40B *)
  mWidth      : int32;       (* Width in pixels *)
  mHeight     : int32;       (* Height in pixels *)

  mColorPlates     : int16;  (* Must be 1 *)
  mBitsPerPixel    : int16;  (* We use 24bpp *)
  mCompression     : uint32; (* We use BI_RGB ~ 0, uncompressed *)
  mImageSize       : uint32; (* mWidth x mHeight x 3B *)
  mHorizRes        : uint32; (* Pixels per meter (75dpi ~ 2953ppm) *)
  mVertRes         : uint32; (* Pixels per meter (75dpi ~ 2953ppm) *)
  mPaletteColors   : uint32; (* Not using palette - 0 *)
  mImportantColors : uint32; (* 0 - all are important *)
}

let header_size =
  B.int32_bytesize * 12 + B.int16_bytesize * 2

let make_header mResX mResY  =
  let int32 = Int32.of_int in {
  mFileSize   = int32 @@ header_size + 2 + mResX * mResX * 3;
  mReserved01 = int32 @@ 0;
  mDataOffset = int32 @@ header_size + 2;
  mHeaderSize = int32 @@ 40;
  mWidth      = int32 @@ mResX;
  mHeight     = int32 @@ mResY;
  mColorPlates     = 1;
  mBitsPerPixel    = 24;
  mCompression     = int32 @@ 0;
  mImageSize       = int32 @@ mResX * mResY * 3;
  mHorizRes        = int32 @@ 2953;
  mVertRes         = int32 @@ 2953;
  mPaletteColors   = int32 @@ 0;
  mImportantColors = int32 @@ 0;
}

let write_header out h =
  B.output_int32 out h.mFileSize;
  B.output_int32 out h.mReserved01;
  B.output_int32 out h.mDataOffset;
  B.output_int32 out h.mHeaderSize;
  B.output_int32 out h.mWidth;
  B.output_int32 out h.mHeight;
  B.output_int16 out h.mColorPlates;
  B.output_int16 out h.mBitsPerPixel;
  B.output_int32 out h.mCompression;
  B.output_int32 out h.mImageSize;
  B.output_int32 out h.mHorizRes;
  B.output_int32 out h.mVertRes;
  B.output_int32 out h.mPaletteColors;
  B.output_int32 out h.mImportantColors

let saveBMP self ?(aGamma=1.) aFilename =
  let open Printf in 
  let bmp = open_out aFilename in
  set_binary_mode_out bmp true;
  fprintf bmp "BM";
  let header = make_header self.mResX self.mResY in
  write_header bmp header;
  let invGamma = 1. /. aGamma in
  for y = 0 to pred self.mResY do
    for x = 0 to pred self.mResX do
      (* bmp is stored from bottom up *)
      let r,g,b = tuple_of_v3 @@ self.mColor.(x + (self.mResY-y-1)*self.mResX) in
      let conv_component x =
        clamp 0 255 (int_of_float @@ (x ** invGamma) *. 255.)
      in
      let bgrB = B.make 3 in
      bgrB.[0] <- Char.chr @@ conv_component b;
      bgrB.[1] <- Char.chr @@ conv_component g;
      bgrB.[2] <- Char.chr @@ conv_component r;
      B.output_bytes bmp bgrB
    done
  done;
  close_out bmp

(*////////////////////////////////////////////////////////////////////////
 * Saving HDR *)
let saveHDR self aFilename =
  let open Printf in 
  let hdr = open_out aFilename in
  set_binary_mode_out hdr true;

  fprintf hdr "#?RADIANCE\n";
  fprintf hdr "# SmallVCM\n";
  fprintf hdr "FORMAT=32-bit_rle_rgbe\n\n";
  fprintf hdr "-Y %d +X %d\n" self.mResY self.mResX;
  for y = 0 to pred self.mResY do
    for x = 0 to pred self.mResX do
      let rgbe = B.make 4 in
      let rgbF = self.mColor.(x + y * self.mResX) in
      let x,y,z = tuple_of_v3 rgbF in
      let v = max x (max y z) in
      if v >= 1e-32
      then (
        let f = 256. /. v in
        let v, e = frexp v in
        let v = v *. f in
        rgbe.[0] <- Char.chr @@ int_of_float (x *. v);
        rgbe.[1] <- Char.chr @@ int_of_float (y *. v);
        rgbe.[2] <- Char.chr @@ int_of_float (z *. v);
        rgbe.[3] <- Char.chr @@ e + 128);
      B.output_bytes hdr rgbe;
    done
  done;
  close_out hdr
