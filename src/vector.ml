
module type S =
sig

    type 'a t

    val data   : 'a t -> 'a array
    val make   : int -> 'a -> 'a t
    val length : 'a t -> int
    val get    : 'a t -> int -> 'a
    val resize : 'a t -> int -> unit
    val set    : 'a t -> int -> 'a -> unit
    val copy   : 'a t -> 'a t
    val blit   : 'a t -> int -> 'a t -> int -> int -> unit

end
  
type 'a t = {mutable data : 'a array; mutable mx : int; default : 'a}

let data v = v.data

let make i e = {data = Array.make i e; mx = 0; default = e}
let of_array a e = {data = a; mx = Array.length a; default = e}
let length v = Array.length v.data
let max v = v.mx
let set_max v m = v.mx <- m
let get v i = if i > v.mx then v.default else Array.get v.data i

let resize v n =
  if n > v.mx
  then (* expand data array *)
    let a = Array.make n v.default in (
      Array.blit v.data 0 a 0 v.mx;
      v.data <- a
    )
  else (* shrink data array *) (
    v.data <- Array.init n (fun i -> get v i);
    v.mx <- n
  )

let set v i e =
  let n = Array.length v.data in
  if i >= n
  then resize v (n * 2);
  if i > v.mx
  then v.mx <- i;
  Array.set v.data i e
            
let copy v = {v with data = Array.copy v.data}
let blit v1 i1 v2 i2 n = 
  if n > Array.length v2.data
  then resize v2 n;
  Array.blit v1.data i1 v2.data i2 n

let fold_left f acc v =
  let acc = ref acc in
  for i = 0 to pred v.mx do
    acc := f !acc (Array.get v.data i)
  done;
  !acc
