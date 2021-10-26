:- module(list_utils, [list_difference/3, concat_list/3, first_element_list_or_empty_list/2,
    concat_set_list/3, map/3, zip/3, unzip/3, zip_const/3]).


% list_difference(List1, List2, Result) List1-List2=Result
list_difference([], _, []) :- !.
list_difference([Item1|List1], List2, [Item1|Difference]) :- 
    not(member(Item1, List2)),
    !,
    list_difference(List1, List2, Difference).
list_difference([_|List1], List2, Difference) :- 
    list_difference(List1, List2, Difference).

concat_list([], X, X).
concat_list([X|Rx], Y, [X|Z]) :- concat_list(Rx, Y, Z).

concat_set_list([], X, X).
concat_set_list([X|Rx], Y, [X|Z]) :- not(member(X, Y)), concat_set_list(Rx, Y, Z), !.
concat_set_list([X|Rx], Y, Z) :- member(X, Y), concat_set_list(Rx, Y, Z).

first_element_list_or_empty_list([],[]).
first_element_list_or_empty_list([F|_],[F]).

zip([], _, []) :- !.
zip(_, [], []).
zip([I1|List1], [I2|List2], [[I1,I2]|Result]) :- zip(List1, List2, Result).

unzip([],[],[]).
unzip([[Item1, Item2]|Rest],[Item1|Rest1],[Item2|Rest2]) :- unzip(Rest, Rest1, Rest2).

zip_const([],_,[]).
zip_const([I1|List1], X, [[I1,X]|Result]) :- zip_const(List1, X, Result).

map(_,[],[]).
map(F,[X|Y],[V|R]):- T =..[F,X,V], call(T), map(F,Y,R).