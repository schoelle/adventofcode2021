# DAY 10

Pascal programming language

# Prerequisites

```console
$ apt-get install fpc
```

# How to run

```console
$ fpc brackets.pas 
Free Pascal Compiler version 3.2.2+dfsg-4 [2021/10/30] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling brackets.pas
fgl.pp(1023,7) Note: Call to subroutine "function TFPGList<System.Int64>.Add(const Item:Int64):LongInt;" marked as inline is not inlined
fgl.pp(1023,20) Note: Call to subroutine "function TFPGList<System.Int64>.Get(Index:LongInt):Int64;" marked as inline is not inlined
brackets.pas(118,7) Note: Call to subroutine "function TFPGList<System.Int64>.Add(const Item:Int64):LongInt;" marked as inline is not inlined
brackets.pas(146,41) Note: Call to subroutine "function TFPGList<System.Int64>.Get(Index:LongInt):Int64;" marked as inline is not inlined
Linking brackets
148 lines compiled, 0.1 sec
4 note(s) issued
$ ./brackets input.txt 
215229
1105996483
```
