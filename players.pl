:- module(players, [random_player/3, console_human_player/3, 
                    ai_player/3, http_player/3, 
                    empty_player_extra_config/2,
                    http_player_extra_config/2]).
:- use_module(list_utils). 
:- use_module('AI/ai_utils'). 
:- use_module('AI/minmax'). 
:- use_module('AI/minmax_utils'). 
:- use_module(run_game_utils).
:- use_module(http_utils).
:- use_module(console_utils).

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
    minmax(step(none,Game,none,continue),result_selection, next_step_generator, terminal_test, sample_utility_function, 2, [Step,_]),
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

http_player_extra_config(ColorPlayer, ExtraGameConfig) :-
    write('Player '), write(ColorPlayer), nl,
    read_with_headline('Host (d if localhost):', HostPrev),
    write(HostPrev), % <- DELETE THIS
    default_if_empty(HostPrev, d, '127.0.0.1', Host),
    write(Host), % <- DELETE THIS
    read_with_headline('Port (d if 9001):', PortPrev),
    default_if_empty(PortPrev, d, 9001, Port),
    concat('/player_',ColorPlayer, PrefixPath),
    ExtraGameConfig = [http_player_config(Host, Port, PrefixPath)].