#use "skip_list.ml";;

let l = make_empty ();;
for i = 1 to 100 do set i (i+1) l done;;
for i = 1 to 100 do unset (i*2) l done;;
valid l;;
entries l;;
