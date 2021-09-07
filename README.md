A functional skip list in ML
============================
This is an implementation of a skip list key/value store in OCaml. The program
is defined in a functional way to prepare it for verification using the Iris
separation logic framework. It does not rely on null pointers.

Skip lists resemble binary trees except that they do not require global
rebalancing. Instead they rely on probabilistic heuristics to keep the time
complexity logarithmic while only applying local modifications to the
datastructure. This enables concurrent usage. Skip lists can be used to
implement sets, key/value mappings, and indexed arrays.
