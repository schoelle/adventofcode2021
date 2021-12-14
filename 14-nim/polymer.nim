import os
import tables
import re
import strformat

if paramCount() != 1:
  echo("Call with a single argument.")
  quit(1)

var
  rules = initTable[string, array[2, string]]()
  counts = initCountTable[string]()
  lastChar: char

for line in lines paramStr(1):
  var parts: array[3, string]
  if match(line, re"^(.)(.) -> (.)$", parts):
    rules[parts[0] & parts[1]] = [parts[0] & parts[2], parts[2] & parts[1]]
  elif line != "":
    lastChar = line[^1]
    for c in 1..len(line)-1:
      counts.inc(fmt"{line[c-1]}{line[c]}")

proc expand() =
  var
    newCounts = initCountTable[string]()
  for i in counts.keys:
    let value = counts[i]
    for v in rules[i]:
      newCounts.inc(v, value)
  counts = newCounts

proc charCounter(): CountTable[char] =
  result = initCountTable[char]()
  for k in counts.keys:
    result.inc(k[0], counts[k])
  # As we always count the first char of a pair, we must not forget the very
  # last character of the sequence (which never changes)
  result.inc(lastChar)
  return result
  
for i in 1..10: # 10 iterations total
  expand()
let res1 = charCounter()
echo(largest(res1).val - smallest(res1).val)

for i in 1..30: # 40 iterations total
  expand()
let res2 = charCounter()
echo(largest(res2).val - smallest(res2).val)
