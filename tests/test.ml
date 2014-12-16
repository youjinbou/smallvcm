open OUnit
open Utils

let v4_to_string v =
  let x,y,z,w = V4.to_tuple v in
  Printf.sprintf "{%f,%f,%f,%f}" x y z w

let v3_to_string v =
  let x,y,z = tuple_of_v3 v in
  Printf.sprintf "{%f,%f,%f}" x y z

let epsilon = epsilon_float *. 20.0

let cmp_float f1 f2 =
  abs_float (f1 -. f2) <= epsilon

let vec4_equal msg v1 v2 =
  for i = 0 to 3 do
    let f1 = V4.get v1 i
    and f2 = V4.get v2 i in
    if not (cmp_float f1 f2) then Printf.printf "%s : coords #%d (%f) (%f) not equal\n" msg i f1 f2
  done

let vec3_equal msg v1 v2 =
  for i = 0 to 2 do
    let f1 = V.get v1 i
    and f2 = V.get v2 i in
    if not (cmp_float f1 f2) then Printf.printf "%s : coords #%d (%f) (%f) not equal\n" msg i f1 f2
  done

let m_equal m1 m2 =
  for i = 0 to 3 do
    let v1 = M.col m1 i
    and v2 = M.col m2 i in
    vec4_equal "m_equal" v1 v2
  done

let dump name t = 
  prerr_endline (name^" {");
  for i = 0 to 3 do
    prerr_endline (v4_to_string (M.row t i))
  done;
  prerr_endline "}"

module Test1 = struct

  (* data generated from smallcvm math code *)

  let readv ic = 
    Scanf.fscanf ic " {%f,%f,%f}" (fun x y z  -> v3_of_tuple (x,y,z))

  let readm ic =
    let readr ic =
      Scanf.fscanf ic " | %f %f %f %f |" (fun x y z w -> V4.of_tuple (x,y,z,w)) 
    and r = M.null () in
    for i = 0 to 3 do
      M.setrow r i (readr ic)
    done;
    r

  let _ =
    let data = open_in "tests/data.txt" in 
    let m = readm data in
    for i = 0 to 99 do
      let m2 = readm data in
      let r = readm data in
      let r' = M.mul m m2 in
      m_equal r r'
    done;
    for i = 0 to 99 do
      let v = readv data in
      let r = readv data in
      let r' = m_apply3 m v in
      vec3_equal "test1 : vec3" r r'
    done

end

module Test2 = struct

  (* data generated using octave *)

  (* read single float *)
  let readf ic =
    Scanf.fscanf ic " %f " (fun f -> f)

  (* read row vector *)
  let readr ic =
    Scanf.fscanf ic " %f %f %f %f " (fun f0 f1 f2 f3 -> V4.of_tuple (f0,f1,f2,f3))  

  (* read column vector *)
  let readc ic =
    let a = Array.init 4 (fun i -> readf ic) in
    V4.of_tuple (a.(0), a.(1), a.(2), a.(3))

  let skip_header ic = ()
  (*
  readk ic;
  readk ic;
  readk ic;
  readk ic *)

  let load_matrix ic =
    skip_header ic;
    let m = M.null () in
    for i = 0 to 3 do
      M.setrow m i (readr ic)
    done;
    m

  let load_vector ic =
    skip_header ic;
    readc ic

  let check_vector m ic =
    let q = load_vector ic in
    let a = load_vector ic in
    let t = m_apply4 m q in
(*
    prerr_endline ("q : " ^ v4_to_string q);
    prerr_endline ("a : " ^ v4_to_string a);
    prerr_endline ("t : " ^ v4_to_string t);
 *)
    vec4_equal "check_vector" a t

  let _ =
    let data = open_in "tests/octave_matrices.txt" in
    for i = 1 to 100 do
      let m1 = load_matrix data in
      let m2 = load_matrix data in
      let r = load_matrix data in
      let t = M.mul m1 m2 in
      m_equal r t
    done;
    let data = open_in "tests/octave_vectors.txt" in 
    let m = load_matrix data in
    for i = 1 to 100 do
      check_vector m data
    done

end

