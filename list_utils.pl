:- module(list_utils, [list_difference/3, concat_list/3, first_element_list_or_empty_list/2,
    concat_set_list/3]).


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
