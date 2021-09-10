(* The indexed skip list using skip sizes. *)

type 'v node = {
  mutable value : 'v;
  forward : (int * 'v node ref option) array;
}

type 'v skip_list = {
  mutable length : int;
  mutable header : (int * 'v node ref option) array;
}

let rec random_level level =
  if Random.bool () then random_level (level + 1) else level

(***
Linear list operations
----------------------
*)

let terminal_skip = (1, None)
let make_empty (u : unit) = { length = 0; header = [| terminal_skip |]; }

let rec node_to_list (size, node_opt) =
  match node_opt with
  | None -> []
  | Some node -> !node.value :: node_to_list !node.forward.(0)

let to_list sl = node_to_list sl.header.(0)

let rec seek i forward level =
  match forward.(level) with
  | (size, None) ->
    if level > 0
      then seek i forward (level - 1)
      else (i - size, None)
  | (size, Some node) ->
    if i - size > 0
      then seek (i - size) !node.forward level
      else if level > 0
        then seek i forward (level - 1)
        else forward.(0)

let rec seek_trace i forward level trace =
  match forward.(level) with
  | (size, None) ->
    trace.(level) <- (i, forward);
    if level > 0
      then seek_trace i forward (level - 1) trace
      else (i - size, None)
  | (size, Some node) ->
    if i - size > 0
      then seek_trace (i - size) !node.forward level trace
      else begin
        trace.(level) <- (i, forward);
        if level > 0
          then seek_trace i forward (level - 1) trace
          else forward.(0)
      end

(* Get the value at some index. *)
let get i sl =
  if i < 0 then raise (Invalid_argument "negative index")
  else let levels = Array.length sl.header in
  match seek (i + 1) sl.header (levels - 1) with
  | (_, None) -> raise (Invalid_argument "index out of bounds")
  | (_, Some node) -> !node.value

(* Set the value at some index. *)
let set i value sl =
  if i < 0 then raise (Invalid_argument "negative index")
  else let levels = Array.length sl.header in
  match seek (i + 1) sl.header (levels - 1) with
  | (_, None) -> raise (Invalid_argument "index out of bounds")
  | (_, Some node) -> !node.value <- value

(* Insert a value at some index. *)
let insert i value sl =
  let levels = Array.length sl.header in
  let level = random_level 0 in
  let trace = Array.make levels (0, [| |]) in
  begin match seek_trace (i + 1) sl.header (levels - 1) trace with
  | (x, None) when x > 0 -> raise (Invalid_argument "index out of bounds") 
  | _ -> ()
  end;
  let node_forward = Array.make (level + 1) terminal_skip in
  let node = ref { value = value; forward = node_forward } in
  (* Re-route forward references and update skip sizes. *)
  Array.iteri (fun i (offset, forward) ->
    let (size, next) = forward.(i) in
    if i > level
      then forward.(i) <- (size + 1, next)
      else begin
        node_forward.(i) <- (size + 1 - offset, next);
        forward.(i) <- (offset, Some node)
      end) trace;
  (* Re-allocate header if needed. *)
  if levels <= level then begin
    let new_levels = Array.make (level + 1 - levels) (i + 1, Some node) in
    sl.header <- Array.append sl.header new_levels
  end;
  sl.length <- sl.length + 1

(* Remove a value at some index. *)
let remove i sl =
  let levels = Array.length sl.header in
  let trace = Array.make levels (0, [| |]) in
  seek_trace (i + 1) sl.header (levels - 1) trace
  (* Remove the next node and update all sizes in the trace. *)

(* Insert a value at the end of the list. *)
let append value sl = insert sl.length value sl

(***
Structural validation
---------------------
*)
