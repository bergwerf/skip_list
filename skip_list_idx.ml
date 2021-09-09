(* The indexed skip list using skip sizes. *)

type 'v node = {
  mutable value : 'v;
  forward : ('v node ref * int) option array;
}

type 'v skip_list = ('v node ref * int) option array ref

(***
Linear list operations
----------------------
*)

let rec node_to_list opt =
  match opt with
  | None -> []
  | Some (node, _) -> !node.value :: node_to_list !node.forward.(0)

let to_list sl = node_to_list !sl.(0)

let rec seek i forward level =
  match forward.(level) with
  | None ->
    if level > 0
      then seek i forward (level - 1)
      else None
  | Some (node, size) ->
    if i - size > 0
      then seek (i - size) !node.forward level
      else if level > 0
        then seek i forward (level - 1)
        else forward.(0)

let rec seek_trace i forward level trace =
  match forward.(level) with
  | None ->
    trace.(level) <- (forward, i);
    if level > 0
      then seek_trace i forward (level - 1) trace
      else None
  | Some (node, size) ->
    if i - size > 0
      then seek_trace (i - size) !node.forward level trace
      else begin
        trace.(level) <- (forward, i);
        if level > 0
          then seek_trace i forward (level - 1) trace
          else forward.(0)
      end

let get i sl =
  if i < 0 then raise (Invalid_argument "negative index")
  else let levels = Array.length !sl in
  match seek i !sl (levels - 1) with
  | None -> raise (Invalid_argument "index out of bounds")
  | Some (node, _) -> !node.value

let set i value sl =
  if i < 0 then raise (Invalid_argument "negative index")
  else let levels = Array.length !sl in
  match seek i !sl (levels - 1) with
  | None -> raise (Invalid_argument "index out of bounds")
  | Some (node, _) -> !node.value <- value

let insert i value sl =
  let levels = Array.length !sl in
  let trace = Array.make levels ([| |], 0) in
  seek_trace i !sl (levels - 1) trace
  (* Now insert the value after the trace. *)

let remove i sl =
  let levels = Array.length !sl in
  let trace = Array.make levels ([| |], 0) in
  seek_trace i !sl (levels - 1) trace
  (* Remove the next node and update all sizes in the trace. *)

(***
Structural validation
---------------------
*)
