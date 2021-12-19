\ Adjust to your matching path, did not find a way to have this looked up
require /usr/share/gforth/0.7.3/string.fs

0 Value xmin
0 Value xmax
0 Value ymin
0 Value ymax

0 Value xpower
0 Value ypower
0 Value hit-count
0 Value height-record
0 Value fd-in
0 Value buf-len

100 Create buf drop


: max-height ( ypower -- height )
    dup 1+ * 2 /
;

: do-y-step ( y ymov -- y' ymov' )
    tuck + swap 1-
;

: do-x-step ( x xmov -- x' xmov' )
    do-y-step 0 max
;

: count-steps ( -- steps )
    0 xpower
    0 ypower
    begin
	do-y-step 2swap
	do-x-step 2swap
	over ymax <=
	4 pick xmin >=
	and
	2 pick ymin <
	or
    until
    over ymin >=
    4 pick xmin >=
    and
    4 pick xmax <=
    and
    if
	ypower max-height
	dup height-record >
	if
	    to height-record
	else
	    drop
	endif
	hit-count 1+ to hit-count
    endif
    2drop
    2drop
;

: open-fire ( -- max-height )
    ymin negate xmax
    begin
	dup
	begin
	    2 pick to ypower
	    dup to xpower
	    count-steps
	    1- dup 0 <=
	until
	drop
	swap 1- swap over ymin <
    until
    2drop
;

\ Parsing an ascii file in this language is terrible
next-arg r/o open-file throw to fd-in
buf 100 fd-in read-line throw drop to buf-len
buf buf-len
61 $split 2swap 2drop
46 $split 2swap evaluate to xmin
46 $split 2swap 2drop
44 $split 2swap evaluate to xmax
61 $split 2swap 2drop
46 $split 2swap evaluate to ymin
46 $split 2swap 2drop
evaluate to ymax 

open-fire
." Problem 1: " height-record . cr
." Problem 2: " hit-count . cr

bye
