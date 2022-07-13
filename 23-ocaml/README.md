# DAY 23

OCAML

# Prerequisites

A current version of OCAML (https://ocaml.org/)

# How to run

```console
$ ocamlopt -o amphipod amphipod.ml
$ ./amphipod
First:     14546 - Rooms: AA BB CC DD Spaces: _ _ _ _ _ _ _
Second:    42308 - Rooms: AAAA BBBB CCCC DDDD Spaces: _ _ _ _ _ _ _
```

# Notes

* I did not implement parsing of the input.txt file. Instead, layout
  of maze and initial configurations are hard-coded in the OCaml code.
  Should be easy to change if you want to run it for different inputs.
* The solution is pretty slow, mostly due to an inefficient
  implementation of the ordered set (using an ordered list and
  searching for duplicated). This causes the overall algorithm to be
  O(n^2) on the number of states that it looks at.
* This was implemented months after the actual AoC. I did not have
  time to implement this at the time, as family Christmas has a higher
  priority.
