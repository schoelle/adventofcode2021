# DAY 4

Eiffel programming language

# Prerequisites

Using EiffelStudio 19.05 as available here:

https://sourceforge.net/projects/eiffelstudio/files/EiffelStudio%2019.05/Build_103187/

Also, for fun I am using by own Matrix library ALGEA as available here:

https://github.com/schoelle/algae/tree/master/library

# How to run

```console
$ ec -config bingo.ecf -full -finalize -c_compile
Eiffel Compilation Manager
Version 19.05.10.3187 GPL Edition - linux-x86-64

Degree 6: Examining System
Degree 5: Parsing Classes
Degree 4: Analyzing Inheritance
Degree 3: Checking Types
Degree 2: Generating Byte Code
Degree -2: Constructing Polymorphic Table
Removing Unused Code
Degree -3: Generating Optimized Code
System Recompiled.
Preparing C compilation
Compiling C code in E1
Compiling C code in C19
Compiling C code in C18
Compiling C code in C17
Compiling C code in C16
Compiling C code in C15
Compiling C code in C14
Compiling C code in C13
Compiling C code in C12
Compiling C code in C11
Compiling C code in C10
Compiling C code in C9
Compiling C code in C8
Compiling C code in C7
Compiling C code in C6
Compiling C code in C5
Compiling C code in C4
Compiling C code in C3
Compiling C code in C2
Compiling C code in C1
Compiling C code in E2
C compilation completed
$ EIFGENs/bingo/F_code/bingo input.txt
Score for first bingo is 64084
Worst board has score 12833
```
