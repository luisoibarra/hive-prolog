:- module(minmax, [minmax/7]).
:- use_module(minmax_utils).

% minmax(InitialState, NextStateGeneratorFunctor, TerminalTestFunctor, UtilityFunctor) 
% InitialState: Root node, initial state.
% ResultSelectionFunctor: ResultSelectionFunctor(State, ResultsFromUtilityList, BestPlayerResult) 
% NextStateGeneratorFunctor: Functor that must take a state and return a list of all possible next states
% TerminalTestFunctor: Functor that must take a state and succeed if is a terminal state
% UtilityFunctor: Functor that receieves a state and returns the gain vector current state
minmax(InitialState, _, _, TerminalTestFunctor, UtilityFunctor, Depth, Result) :-
    (
        Depth = 0
        ;
        TerminalTest =.. [TerminalTestFunctor, InitialState],
        call(TerminalTest)
    ),
    !,
    UtilityFun =.. [UtilityFunctor, InitialState, Result],
    call(UtilityFun).

minmax(InitialState, ResultSelectionFunctor, NextStateGeneratorFunctor, TerminalTestFunctor, UtilityFunctor, Depth, Result) :-
    Depth >= 0,
    % Not Terminal
    GenerateAll =.. [NextStateGeneratorFunctor, InitialState, AllNextStates],
    call(GenerateAll),
    NextDepth is Depth - 1,
    findall([Y,X], (member(Y, AllNextStates), 
                minmax(Y, ResultSelectionFunctor, NextStateGeneratorFunctor, TerminalTestFunctor, UtilityFunctor, NextDepth, X))
            ,Results),
    Selector =.. [ResultSelectionFunctor, InitialState, Results, Result],
    call(Selector).


% two_minimax(InitialState, MaxMinState, Alpha, Beta, ResultSelectionFunctor, NextStateGeneratorFunctor, TerminalTestFunctor, UtilityFunctor, Depth, Result)