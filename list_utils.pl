:- module(list_utils, [list_difference/3, concat_list/3, first_element_list_or_empty_list/2,
    concat_set_list/3, map/3, zip/3, unzip/3, zip_const/3, element_at/3,
    remove_at/3, remove_all/3, get_random_element/2, maxim/4, exchange_elements/4]).
:- use_module('IA/minmax_utils').

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

element_at(List, Index, Element) :- 
    integer(Index), !,
    element_at_int(List, Index,Element).
element_at(List, Index, Element) :-
    element_at_not_int(List, 0, Index, Element).

element_at_not_int([Element|_], CurrentIndex, CurrentIndex, Element). 
element_at_not_int([_|Rest], CurrentIndex, Index, Element) :-
    NewIndex is CurrentIndex+1,
    element_at_not_int(Rest, NewIndex,Index,Element).

element_at_int([Element|_], 0, Element) :- !. 
element_at_int([_|List], Index, Element) :- 
    NewIndex is Index - 1, 
    element_at_int(List, NewIndex, Element). 

remove_at([_|List], 0, List) :- !.
remove_at([X|List], Index, [X|Result]) :-
    NewIndex is Index - 1, 
    remove_at(List, NewIndex, Result).

remove_all(List, Pattern, NewList) :-
    findall(X, (member(X,List), X=Pattern), NotWanted),
    list_difference(List, NotWanted, NewList).

get_random_element(List, Element) :-
    length(List, Length),
    Length > 0,
    Pos is random(Length),
    element_at(List, Pos, Element).

maxim([Default|List], ComparerFunctor, ComparerPreArgs, Min) :-
    concat_list([ComparerFunctor], ComparerPreArgs, FunctorList), 
    aux_maxim(List, Default, FunctorList, Min).
aux_maxim([], CurrentMin, _, CurrentMin).
aux_maxim([Current|List], CurrentMin, PreComparerFunctorList, Min) :-
    concat_list(PreComparerFunctorList, [Current, CurrentMin], ComparerFunctorList),
    Comparer =.. ComparerFunctorList,
    (
        call(Comparer),
        aux_maxim(List, Current, PreComparerFunctorList, Min)
        ;
        not(call(Comparer)),
        aux_maxim(List, CurrentMin, PreComparerFunctorList, Min)
    ).

exchange_elements([],_,_,[]).
exchange_elements([Element1|List], Element1, Element2, [Element2|ResultList]) :- 
    exchange_elements(List, Element1, Element2, ResultList), !.
exchange_elements([Element|List], Element1, Element2, [Element|ResultList]) :- 
    Element \= Element1,
    exchange_elements(List, Element1, Element2, ResultList).
