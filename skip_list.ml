(* The skip_list based on "A Skip List Cookbook". *)

(* This is based on a probability of 1/2 and a maximum of 2^32 elements. *)
let max_levels = 32

type ('k, 'v) node = {
  key : 'k;
  mutable value : 'v;
  forward : ('k, 'v) node ref array;
}

(* The header is extended when needed to avoid null pointers. *)
type ('k, 'v) skip_list = ('k, 'v) node ref array ref

(* Make an empty list. *)
let make_empty (u : unit) = ref [| |]

(* Seek the node with key k, starting at the given skip level. *)
let rec seek k forward level =
  let next = !(forward.(level)) in
  if next.key < k && Array.length next.forward > 0
    then seek k next.forward level
  else if level > 0
    then seek k forward (level - 1)
  else !(forward.(0))

(* Get the value associated with a given key. *)
let get key sl =
  let levels = Array.length !sl in
  if levels = 0 then None else
  let node = seek key !sl (levels - 1) in
  if node.key = key then Some node.value else None

(* Same as seek, but stores visited forward arrays in a trace. *)
let rec seek_trace k forward level trace =
  let next = !(forward.(level)) in
  if next.key < k && Array.length next.forward > 0
    then seek_trace k next.forward level trace
  else if level > 0 then begin
    trace.(level) <- forward;
    seek_trace k forward (level - 1) trace
  end else !(forward.(0))

(* Determine a random level. *)
let rec random_level (level : int) =
  if level < max_levels && Random.bool ()
  then random_level (level + 1)
  else level

(* Insert a key/value pair after the update trace. *)
let insert key value sl trace =
  let level = random_level 0 in
  let levels = Array.length !sl in
  let subtrace = Array.sub trace 0 (min (level + 1) levels) in
  let forward = Array.mapi (fun i forward -> forward.(i)) subtrace in
  let node = ref { key = key; value = value; forward = forward } in
  Array.iteri (fun i forward -> forward.(i) <- node) subtrace;
  if levels <= level then begin
    let new_levels = Array.make (level - levels + 1) node in
    sl := Array.append !sl new_levels
  end

(* Update or insert a key/value pair. *)
let set key value sl =
  let trace = Array.make (Array.length !sl) !sl in
  let levels = Array.length !sl in
  if levels = 0 then insert key value sl trace
  else begin
    let node = seek_trace key !sl (levels - 1) trace in
    if node.key = key then node.value <- value
    else insert key value sl trace
  end
