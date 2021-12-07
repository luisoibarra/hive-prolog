:- module(players, [random_player/3, console_human_player/3, 
                    ai_player/3, http_player/3, 
                    empty_player_extra_config/2]).
:- use_module('../AI/ai_utils'). 
:- use_module('../AI/minmax'). 
:- use_module('../AI/minmax_utils'). 
:- use_module('../AI/utility_function'). 
:- use_module(run_game_utils).
:- use_module('../HTTP/http_utils').
:- use_module('../Utils/list_utils'). 
:- use_module('../Utils/console_utils').

% empty_player_extra_config(ColorPlayer, ExtraGameConfig) Returns an empty ExtraGameConfig list
empty_player_extra_config(_, []).

% random_player(Game, GameConfig, Action) Returns a random action to be played
random_player(Game, _, Action) :- 
    game([],_,_) = Game,
    findall(A, (A = set_play(Pos, 4, 3), make_a_play(A, Game, _, _, _), nonvar(Pos)), Actions),
    get_random_element(Actions, Action).
random_player(Game, _, Action) :- 
    game([_|_],_,_) = Game,
    all_next_game_steps(Game, Steps),
    findall(A, (member(Step, Steps), step(A, _, _, Status) = Step, Status \= invalid), Actions),
    get_random_element(Actions, Action).

% ai_player(Game, GameConfig, Action) Returns an action selected with an AI algorithm
ai_player(Game, GameConfig, Action) :-
    game([],_,_) = Game,
    random_player(Game, GameConfig, Action).
ai_player(Game, _, Action) :-
    game(_, Player, _) = Game,
    two_minimax(step(none, Game, none, continue), 0, Player, -100000000, 100000000,two_result_selection,next_step_generator,terminal_test,player_utility_function,1,[Step,_]),
    % two_minimax(step(none, Game, none, continue), -100000000, 100000000,result_selection,next_step_generator,terminal_test,sample_two_player_utility_function,2,[Step,_]),
    % minmax(step(none,Game,none,continue),result_selection, next_step_generator, terminal_test, player_utility_function, 2, [Step,_]),
    step(Action, _, _, _) = Step.

% console_human_player(Game, GameConfig, Action) Implements the console interface and input for a human player
console_human_player(_, _, Action) :-
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

% http_player(Game, ExtraGameConfig, Action) Implements a HTTP player interface
http_player(Game, ExtraGameConfig, Action) :-
    http_get_action(Game, ExtraGameConfig, Action).
