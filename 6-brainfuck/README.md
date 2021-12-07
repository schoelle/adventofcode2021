# DAY 5

Brainfuck esoteric language

# Background

This is my attempt of solving today's problem using the Brainfuck
language. As expected, the result is not very readable.

The code ONLY solves the first problem. While the second problem is
like the first just with more iterations, the exponential run-time of
the Brainfuck program makes it impossible to compute a solution within
reasonable time.

The script needs some input/output processing, which is done using awk
and wc. Parsing comma-separated integers in Brainfuck is an exercise
left to the reader.

The program is written in a style that works with 8 bit cell sizes and
assumes zero to signal EOF.

# Prerequisites

* A brainfuck compiler. I have used the tritium compiler available at
  https://github.com/rdebath/Brainfuck - the script expects the binary
  to be in the path and called `bfi`.
* A shell with `awk` and `wc` readily available.

# How to run

```bash
$ ./run.sh input.txt
```

# How it is implemented

The first byte is the iteration count. Then follows the actual array,
with two extra bytes to allow copying, moving and not operations. Here
is an illustration of the initial memory layout for the test data and
18 iterations.

```
[<12> 0A 04 00 00 05 00 00 04 00 00 02 00 00 03 00 00 0A]
```

Please see the code comments for more details.

# Performance

On my XPS 15 9500 laptop using an Intel i7-10750H processor, the
program takes around 140 seconds to finish.
