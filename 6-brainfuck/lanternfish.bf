Number of iterations: 80
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++

10 to mark startof data
>++++++++++

Read in input always leaving two spaces
>,[+>>>,]

10 to mark end of data
++++++++++

Rewindo to start
<----------[++++++++++<----------]++++++++++<

Here is the main loop
[
>>

for each iteration we do the following steps

Travers elements to decrease count and check for zero
----------[++++++++++
  Decrease by 1
  -
  Copy and move
  [->+>+<<]>>[-<<+>>]<<
  Go to next
  >
  Negate
  >+<[[-]>-<]>[<+>-]<
>>----------]++++++++++

Rewind to start
<----------[++++++++++<----------]++++++++++>

Traverse elements to create new fish
----------[++++++++++
  >
  if we were zero
  [
  mark location with 11 by adding 10
  ++++++++++
  forward to end
  ----------[++++++++++>----------]++++++++++
  add new element
  ->>>++++++++++
  back to 11 and remove it
  -----------[+++++++++++<-----------]
  ]
>>----------]++++++++++

Rewind to start
<----------[++++++++++<----------]++++++++++>

Travers elements to decrease count and check for zero once more
----------[++++++++++
  Copy and move
  [->+>+<<]>>[-<<+>>]<<
  Go to next
  >
  Negate
  >+<[[-]>-<]>[<+>-]<
>>----------]++++++++++

Rewind to start
<----------[++++++++++<----------]++++++++++>

Traverse elements to set the zero ones to 7
----------[++++++++++
>
[-<+++++++>]
>>----------]++++++++++

Rewind to start and repeat main loop
<----------[++++++++++<----------]++++++++++<-
]

>>
Traverse elements
----------[++++++++++
print exclamation mark
[-]
+++++++++++++++++++++++++++++++++.
>>>----------]++++++++++
