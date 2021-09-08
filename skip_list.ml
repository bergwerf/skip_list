(* The skip_list based on "A Skip List Cookbook". *)

type ('k, 'v) node = {
  key : 'k;
  mutable value : 'v;
  forward : ('k, 'v) node ref array ref;
}

(*
!IMPORTANT!

This algorithm does not seek properly because it does not skip to terminal nodes
with shorter forward arrays. This problem was introduced to avoid null pointers.
The type of the forward array must be changed into `node ref option array`. When
seeking the trace will have type `node ref option array array`, and the output
should be `node option`. I will not limit the number of levels, which would
avoid the need to reallocate the header, in order to realize the theoretical
asymptotic complexity.
*)

(* The header is extended when needed to avoid null pointers. *)
type ('k, 'v) skip_list = ('k, 'v) node ref array ref

(* Make an empty list. *)
let make_empty (u : unit) = ref [| |]

(* Validate starting at a previous value and a forward array. *)
let rec valid_forward prev forward = Array.for_all (fun node ->
  prev < !node.key && valid_forward !node.key !node.forward) !forward

(* Validate starting at a header . *)
let valid sl = Array.for_all (fun node ->
  valid_forward !node.key !node.forward) !sl

(* Seek the node with key k, starting at the given skip level. *)
let rec seek k forward level =
  let next = !(!forward.(level)) in
  if next.key < k && Array.length !(next.forward) > level
    then seek k next.forward level
  else if level > 0
    then seek k forward (level - 1)
  else !forward.(0)

(* Same as seek, but stores visited forward arrays in a trace. *)
let rec seek_trace k forward level trace =
  let next = !(!forward.(level)) in
  if next.key < k && Array.length !(next.forward) > level
    then seek_trace k next.forward level trace
  else begin
    trace.(level) <- forward;
    if level > 0 then seek_trace k forward (level - 1) trace
    else !forward.(0)
  end

(* Lookup a key. *)
let get key sl =
  let levels = Array.length !sl in
  if levels = 0 then None else
  let node = !(seek key sl (levels - 1)) in
  if node.key = key then Some node.value else None

(* Determine a random level. *)
let rec random_level level =
  if Random.bool () then random_level (level + 1) else level

(* Insert a key/value pair after the update trace. *)
let insert key value sl trace =
  let level = random_level 0 in
  let levels = Array.length !sl in
  let subtrace = Array.sub trace 0 (min (level + 1) levels) in
  let forward = Array.mapi (fun i fwd -> !fwd.(i)) subtrace in
  let node = ref { key = key; value = value; forward = ref forward } in
  Array.iteri (fun i fwd -> !fwd.(i) <- node) subtrace;
  if levels <= level then begin
    let new_levels = Array.make (level - levels + 1) node in
    sl := Array.append !sl new_levels
  end

(* Update or insert a key/value pair. *)
let set key value sl =
  let trace = Array.make (Array.length !sl) sl in
  let levels = Array.length !sl in
  if levels = 0 then insert key value sl trace
  else begin
    let node = !(seek_trace key sl (levels - 1) trace) in
    if node.key = key then node.value <- value
    else insert key value sl trace
  end

(* Make a forward array bypass a node at level i. *)
let bypass i forward node =
  if !forward.(i) == node then
    let node_forward = !(!node.forward) in
    if Array.length node_forward > i
    then !forward.(i) <- node_forward.(i)
    else forward := Array.sub !forward 0 i

(* Delete a key. *)
let delete key sl =
  let trace = Array.make (Array.length !sl) sl in
  let levels = Array.length !sl in
  if levels = 0 then false else
    let node_ref = seek_trace key sl (levels - 1) trace in
    if !node_ref.key != key then false
    else (Array.iteri (fun i fwd -> bypass i fwd node_ref) trace; true)
