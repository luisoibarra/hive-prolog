:- module(list_utils, [list_difference/3, concat_list/3, first_element_list_or_empty_list/2]).


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

first_element_list_or_empty_list([],[]).
first_element_list_or_empty_list([F|_],[F]).
