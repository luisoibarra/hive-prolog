:- module(minmax_utils, [result_selection/3, next_step_generator/2, terminal_test/1,
    utility_function/3, utility_for_player/4, sample_utility_function/2, maxim_fuc/3,
    max/3, min/3, sample_two_player_utility_function/2, two_maxim_fuc/2, two_minim_fuc/2, two_result_selection/4]).
:- use_module('../Utils/list_utils').
:- use_module('../Run/run_game_utils').
:- use_module(ai_utils).

player_index(0,white).
player_index(1,black).

max(Item1, Item2, Item1) :-
    Item1 >= Item2, !.
max(_, Item2, Item2).
min(Item1, Item2, Item1) :-
    Item1 =< Item2, !.
min(_, Item2, Item2).

% maxim_fuc(Player, [step(...),[step(...),VectorValue1]], [step(...),[step(...),VectorValue2]]) Compares which Argument is greater according to Player
maxim_fuc(Player, [_,[_,V1]], [_,[_,V2]]) :-
    player_index(Index, Player),
    element_at(V1, Index, Value1),
    element_at(V2, Index, Value2),
    Value1 > Value2.

% result_selection(State, VectorList, Result) Returns the maxim value in VectorList according maxin_fun
result_selection(State, VectorList, Result) :- 
    step(_,game(_,Player, _),_,_) = State,
    maxim(VectorList, maxim_fuc, [Player], [Step,[_,Vector]]),
    [Step,Vector] = Result.

two_maxim_fuc([_,[_,V1]], [_,[_,V2]]) :-
    V1 > V2.

two_minim_fuc([_,[_,V1]], [_,[_,V2]]) :-
    V1 < V2.

% two_result_selection(State, VectorList, Result) Returns the maxim value in VectorList according maxin_fun
two_result_selection(_, MaxMinState, VectorList, Result) :- 
    (
        MaxMinState = min,
        maxim(VectorList, two_minim_fuc, [], [Step,[_,Vector]])
        ;
        MaxMinState = max,
        maxim(VectorList, two_maxim_fuc, [], [Step,[_,Vector]])
    ),
    findall([S, V], (member([S, [_, V]], VectorList), V=Vector), Options),
    random_member(Result, Options).
    

% next_step_generator(State, NextStates) Returns all posible NextStates from State
next_step_generator(State, NextStates) :- 
    step(_, Game, _, _) = State,
    all_next_game_steps(Game, Steps),
    findall(Step, (member(Step, Steps), step(_, _, _, Status) = Step, Status \= invalid), NextStates).

% terminal_test(State) Succeed if State is a final game state
terminal_test(State) :- 
    step(_, _, _, Status) = State,
    (Status = over ; Status = tie).

% utility_function(State, UtilityFunctor, Vector) Return a Vector corresponding to the UtilityFunctor resutl for each player
% Used as an auxiliary function that calculates the utility vector
% UtilityFunctor: UtilityFunctor(State, Player, UtilityPlayerValue)
utility_function(State, UtilityFunctor, Vector) :-
    step(_,Game, _, _) = State,
    get_game_PiecesInfo(Game, PiecesInfo),
    utility_for_player(State, UtilityFunctor, PiecesInfo, Vector).

% utility_for_player(State, UtilityFunctor, PiecesInfo, UtilityVector)
% Auxiliary function that fills the UtilityVector with the values returned from UtilityFunctor
% UtilityFunctor: UtilityFunctor(State, Player, UtilityPlayerValue)
% PiecesInfo: List of pieces_info(Player,_).
utility_for_player(_, _, [], []).
utility_for_player(State, UtilityFunctor, [PiecesInfo|Rest], [CurrentValue|Vector]) :-
    PiecesInfo = pieces_info(Player,_),
    UtilityFunction =.. [UtilityFunctor, State, Player, CurrentValue],
    call(UtilityFunction),
    utility_for_player(State, UtilityFunctor, Rest, Vector).

% sample_utility_function(State, Vector) Simple utility function
sample_utility_function(State, Vector) :-
    step(Action,_, _, _) = State,
    utility_function(State, sample_utility_player_function, Value),
    Vector = [Action,Value].

sample_utility_player_function(State, Player, Value) :- 
    step(_,Game, _, _) = State,
    get_game_PiecesInfo(Game, PieceInfos),
    member(pieces_info(Player, Pieces), PieceInfos),
    length(Pieces, Length),
    % Value is Length. % Allways try to move
    Value is -Length. % Allways try to set 

sample_two_player_utility_function(State, Vector) :-
    step(Action,_, _, _) = State,
    player_index(0,Player), % Always the first player due to zero sum game
    sample_utility_player_function(State, Player, Value),
    Vector = [Action, Value].