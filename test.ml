(* 1. Test the regular skip list. *)

#use "skip_list.ml";;

let l = make_empty ();;
for i = 0 to 100 do set i (i+1) l done;;
for i = 0 to 100 do delete (i*2) l done;;
valid l;;
entries l;;

(* 2. Test the indexed skip list. *)

#use "skip_list_idx.ml";;

let l = make_empty ();;
for i = 0 to 100 do append i l done;;
for i = 0 to 100 do insert (i*2) i l done;;
for i = 0 to 100 do delete i l done;;
valid l;;
to_list l;;
