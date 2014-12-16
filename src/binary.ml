open Utils
include String (* Byte *)

let make k =
  make k '\000'

let int64_bytesize = 8
let int32_bytesize = 4
let int16_bytesize = 2

let output_bytes chan bytes =
  output_string chan bytes

let set_binstring_of_int64 x b =
  for i = 0 to pred int64_bytesize do
    b.[i] <- Char.chr @@ (Int64.to_int (Int64.shift_right_logical x (i * 8))) land 0xFF
  done

let set_binstring_of_float f b =
  let x = Int64.bits_of_float f in
  set_binstring_of_int64 x b

let set_binstring_of_int32 x b =
  for i = 0 to pred int32_bytesize do
    b.[i] <- Char.chr @@ (Int32.to_int (Int32.shift_right_logical x (i * 8))) land 0xFF
  done

let set_binstring_of_int16 x b =
  for i = 0 to 1 do
    b.[i] <- Char.chr @@ (x lsr (i * 8)) land 0xFF
  done

let set_binstring_of_int x b =
  let i = Int32.of_int x in
  set_binstring_of_int32 i b

let output_int16 f i =
  let b = make int16_bytesize in
  set_binstring_of_int16 i b;
  output_bytes f b

let output_int32 f i =
  let b = make int32_bytesize in
  set_binstring_of_int32 i b;
  output_bytes f b

let output_int64 f i =
  let b = make int64_bytesize in
  set_binstring_of_int64 i b;
  output_bytes f b
