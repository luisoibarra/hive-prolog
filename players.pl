:- module(players, [random_player/2, console_human_player/2, ia_player/2]).
:- use_module(list_utils). 
:- use_module('IA/ia_utils'). 
:- use_module('IA/minmax'). 
:- use_module('IA/minmax_utils'). 
:- use_module(run_game_utils).
:- use_module(console_utils).


% random_player(Game, Action) Returns a random action to be played
random_player(Game, Action) :- 
    game([],_,_) = Game,
    findall(A, (A = set_play(Pos, 4, 3), make_a_play(A, Game, _, _, _), nonvar(Pos)), Actions),
    get_random_element(Actions, Action).
random_player(Game, Action) :- 
    game([_|_],_,_) = Game,
    all_next_game_steps(Game, Steps),
    findall(A, (member(Step, Steps), step(A, _, _, Status) = Step, Status \= invalid), Actions),
    get_random_element(Actions, Action).

ia_player(Game, Action) :-
    game([],_,_) = Game,
    random_player(Game, Action).
ia_player(Game, Action) :-
    minmax(step(none,Game,none,continue),result_selection, next_step_generator, terminal_test, sample_utility_function, 2, [Action,_]).

% console_human_player(Game, Action) Implements the console interface and input for a human player
console_human_player(Game, Action) :-
    write('1: For place a piece'), nl, 
    write('2: For move a piece'), nl,
    read_with_headline('', Option),
    (
        1 = Option,
        read_with_headline('Select piece to place:', PositionPieceTypeAdd),
        write('Write the position'), nl,
        read_position(PosX, PosY),
        Action = set_play(PositionPieceTypeAdd, PosX, PosY)
        ;
        2 = Option,
        write('Write the position of the piece to move'), nl,
        read_position(PosX, PosY),
        write('Write the detination position'), nl,
        read_position(DestPosX, DestPosY),
        Action = move_play(PosX, PosY,DestPosX, DestPosY)
    ).