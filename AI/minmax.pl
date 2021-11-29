:- module(minmax, [minmax/7, two_minimax/9]).
:- use_module(minmax_utils).
:- use_module(utility_function).

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

minmax_player(white, max).
minmax_player(black, min).

% Specific minmax algorithm for two players with alpha beta pruning
two_minimax(InitialState, Alpha, Beta, ResultSelectionFunctor, NextStateGeneratorFunctor, TerminalTestFunctor, UtilityFunctor, Depth, Result) :-
    (
        Depth = 0
        ;
        TerminalTest =.. [TerminalTestFunctor, InitialState],
        call(TerminalTest)
    ),
    !,
    UtilityFun =.. [UtilityFunctor, InitialState, Result],
    call(UtilityFun).

two_minimax(InitialState, Alpha, Beta, ResultSelectionFunctor, NextStateGeneratorFunctor, TerminalTestFunctor, UtilityFunctor, Depth, Result) :-
    step(_, game(_,Player,_), _, _) = InitialState,
    minmax_player(Player, MaxMinState),

    GenerateAll =.. [NextStateGeneratorFunctor, InitialState, AllNextStates],
    call(GenerateAll),
    NextDepth is Depth - 1,
    (
        (
            MaxMinState = max,
            
            assertz(two_mm_state(InitialState, Depth, MaxMinState, Alpha)),
            assertz(two_mm_continue(InitialState, Depth, MaxMinState)),

            findall([State, X],
                (
                    member(State, AllNextStates),
                    
                    two_mm_state(InitialState, Depth, MaxMinState, CurrentAlpha), % Get Current Alpha
                    two_mm_continue(InitialState, Depth, MaxMinState), % If the evaluation should continue

                    two_minimax(State, CurrentAlpha, Beta, ResultSelectionFunctor, NextStateGeneratorFunctor, TerminalTestFunctor, UtilityFunctor, NextDepth, X),
                    retract(two_mm_state(InitialState, Depth, MaxMinState, CurrentAlpha)),
                    X = [_,Value],
                    max(Value, CurrentAlpha, NewAlpha),
                    assertz(two_mm_state(InitialState, Depth, MaxMinState, NewAlpha)),
                    
                    (
                        Value >= Beta,
                        retract(two_mm_continue(InitialState, Depth, MaxMinState)) % Stop further evaluation
                        ;
                        Value < Beta
                    )
                ),
                States
            ),
            retract(two_mm_state(InitialState, Depth, MaxMinState, _)), % Clean Database
            (
                retract(two_mm_continue(InitialState, Depth, MaxMinState)) % Stop further evaluation
                ;
                true
            ),
            Selector =.. [ResultSelectionFunctor, InitialState, States, Result],
            call(Selector)
        )
        ;
        (
            MaxMinState = min,

            assertz(two_mm_state(InitialState, Depth, MaxMinState, Beta)),
            assertz(two_mm_continue(InitialState, Depth, MaxMinState)),

            findall([State, X],
                (
                    member(State, AllNextStates),
                    
                    two_mm_state(InitialState, Depth, MaxMinState, CurrentBeta), % Get Current Alpha
                    two_mm_continue(InitialState, Depth, MaxMinState), % If the evaluation should continue

                    two_minimax(State, Alpha, CurrentBeta, ResultSelectionFunctor, NextStateGeneratorFunctor, TerminalTestFunctor, UtilityFunctor, NextDepth, X),
                    retract(two_mm_state(InitialState, Depth, MaxMinState, CurrentBeta)),
                    X = [_,Value],
                    min(Value, CurrentBeta, NewBeta),
                    assertz(two_mm_state(InitialState, Depth, MaxMinState, NewBeta)),
                    (
                        Value =< Alpha,
                        retract(two_mm_continue(InitialState, Depth, MaxMinState)) % Stop further evaluation
                        ;
                        Value > Alpha
                    )
                ),
                States
            ),
            retract(two_mm_state(InitialState, Depth, MaxMinState, _)), % Clean Database
            (
                retract(two_mm_continue(InitialState, Depth, MaxMinState)) % Stop further evaluation
                ;
                true
            ),
            Selector =.. [ResultSelectionFunctor, InitialState, States, Result],
            call(Selector)

        )
    ).