#!/usr/bin/env swipl
read_file(Stream,[]) :-
    at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.

split_data([],[]).
split_data([X|L],[P|Locations]) :-
    split_string(X,"-","",P),
    split_data(L,Locations).

from_to(_,_,[],false).
from_to(From,To,[[From,To]|_],true).
from_to(From,To,[[To,From]|_],true).
from_to(From,To,[_|X],V) :-
    from_to(From,To,X,V).

is_capital(X) :-
    atom_codes(X,[C|_]),C > 64,C < 91.

all_paths(To,To,_,_,[To],_).
all_paths(From,To,Connections,Visited,[From|L],false) :-
    from_to(From,X,Connections,true),
    \+ is_capital(X),
    \+ X == "end",
    \+ member(X,Visited),
    all_paths(X,To,Connections,Visited,L,true).
all_paths(From,To,Connections,Visited,[From|L],D) :-
    from_to(From,X,Connections,true),
    \+ is_capital(X),
    \+ member(X,Visited),
    all_paths(X,To,Connections,[X|Visited],L,D).
all_paths(From,To,Connections,Visited,[From|L],D) :-
    from_to(From,X,Connections,true),
    is_capital(X),
    \+ member(X,Visited),
    all_paths(X,To,Connections,Visited,L,D).

list_all_paths(Connections,From,To,Double) :-
    setof(C, all_paths(From,To,Connections,[From],C,Double), Cs),
    length(Cs,L),
    write(L),nl.

main([Filename]) :-
    open(Filename, read, Str),
    read_file(Str,Lines),
    close(Str),
    split_data(Lines,Connections),
    list_all_paths(Connections,"start","end",true),
    list_all_paths(Connections,"start","end",false).

:- initialization(main, main).
