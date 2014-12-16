open Rng

let _ =
  let module M = Legacy in
  let s = M.make 1234 in
  for i = 0 to pred 100 do
    Printf.printf "%.7f\n" (M.getFloat s)
  done
