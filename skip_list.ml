(* The skip_list based on "A Skip List Cookbook". *)

type ('k, 'v) node = {
  key : 'k;
  mutable value : 'v;
  forward : ('k, 'v) node ref option array;
}

(* The header is reallocated when more levels are needed. *)
type ('k, 'v) skip_list = ('k, 'v) node ref option array ref

(***
Map operations
--------------
I believe no locking is needed at all to keep the skip list intact during each
operation. To achieve this the order of operations must be taken into account,
such that each element of the skip list remains reachable in each intermediary
state. It becomes more tricky when concurrent processes insert new nodes next to
each other.
*)

(* Make an empty skip list. *)
let make_empty (u : unit) = ref [| None |]

(* Get all key/value pairs starting at a `node ref option`. *)
let rec get_entries node_opt =
  match node_opt with
  | None -> []
  | Some node -> (!node.key, !node.value) :: get_entries !node.forward.(0)

(* Get all key/value pairs. *)
let entries sl = get_entries !sl.(0)

(* Seek the node with key k, starting at the given skip level. *)
let rec seek k forward level =
  match forward.(level) with
  | None ->
    if level > 0 then seek k forward (level - 1)
    else None
  | Some node ->
    if !node.key < k then seek k !node.forward level
    else if level > 0 then seek k forward (level - 1)
    else forward.(0)

(* Same as seek, but stores visited forward arrays in a trace. *)
let rec seek_trace k forward level trace =
  match forward.(level) with
  | None ->
    trace.(level) <- forward;
    if level > 0 then seek_trace k forward (level - 1) trace
    else None
  | Some node ->
    if !node.key < k then seek_trace k !node.forward level trace
    else begin
      trace.(level) <- forward;
      if level > 0 then seek_trace k forward (level - 1) trace
      else forward.(0)
    end

(* Lookup a key. *)
let get key sl =
  let levels = Array.length !sl in
  match seek key !sl (levels - 1) with
  | None -> None
  | Some node -> if !node.key = key then Some !node.value else None

(* Determine a random level. *)
let rec random_level level =
  if Random.bool () then random_level (level + 1) else level

(* Insert a key/value pair using its seek trace. *)
let insert key value sl trace =
  let level = random_level 0 in
  let levels = Array.length !sl in
  let subtrace = Array.sub trace 0 (min (level + 1) levels) in
  let node_forward = Array.make (level + 1) None in
  let node = ref { key = key; value = value; forward = node_forward } in
  Array.iteri (fun i forward ->
    node_forward.(i) <- forward.(i);
    forward.(i) <- Some node) subtrace;
  if levels <= level then begin
    let new_levels = Array.make (level - levels + 1) (Some node) in
    sl := Array.append !sl new_levels
  end

(* Update or insert a key/value pair. *)
let set key value sl =
  let levels = Array.length !sl in
  let trace = Array.make levels [| |] in
  match seek_trace key !sl (levels - 1) trace with
  | None -> insert key value sl trace
  | Some node ->
    if !node.key = key then !node.value <- value
    else insert key value sl trace

(* Delete a node using its seek trace. *)
let delete node trace =
  Array.iteri (fun i forward ->
    begin match forward.(i) with
    | None -> ()
    | Some n -> if n == node then forward.(i) <- !node.forward.(i)
    end) trace

(* Remove a key. *)
let unset key sl =
  let levels = Array.length !sl in
  let trace = Array.make levels [| |] in
  match seek_trace key !sl (levels - 1) trace with
  | None -> ()
  | Some node -> if !node.key = key then delete node trace

(***
Structural validation
---------------------
The algorithms described here maintain an order between pointers of different
levels in each forward array. Lower level pointers never point further than
higher level pointers. But this condition is not strictly required for a
functioning skip list. It is possible to fragment the skip list, for example by
removing level zero skips, making some nodes unreachable. A strong validity
requirement could state that every forward pointer is reachable by a seek
operation, and is in fact needed for seeking after zero or more nodes are
removed. This ensures the algorithm never stores useless pointers.
*)

(* Ordering of forward pointers (else seeking gets stuck). *)
let node_ref_opt_leq x y =
  match x, y with
  | _, None -> true
  | Some u, Some v -> !u.key <= !v.key
  | _, _ -> false

(* Check if a list is leq-sorted. *)
let rec is_sorted leq l =
  match l with
  | [] -> true
  | [_] -> true
  | x :: y :: tl -> leq x y && is_sorted leq (y :: tl)

(* Validate a `node ref option`. *)
let rec valid_node prev_key node_opt =
  match node_opt with
  | None -> true
  | Some node ->
    let key = Some !node.key in
    prev_key < key && valid_node key !node.forward.(0) &&
    is_sorted node_ref_opt_leq (Array.to_list !node.forward) &&
    Array.for_all (fun b -> b) (Array.mapi (fun i opt ->
      match opt with
      | None -> true
      | Some next -> !node.key < !next.key && i < Array.length !next.forward
      ) !node.forward)

(* Validate a skip list. *)
let valid sl =
  is_sorted node_ref_opt_leq (Array.to_list !sl) &&
  valid_node None !sl.(0)
