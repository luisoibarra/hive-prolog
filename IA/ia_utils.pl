:- module(ia_utils, [next_game_step/2, all_next_game_steps/2]).
:- use_module('../run_game_utils').

% next_game_step(Game, Action, NewGame, Feedback, Status) Given a Game returns a posible Action NewGame Feedback Status
next_game_step(Game, step(Action, NewGame, Feedback, Status)) :-
    make_a_play(Action, Game, NewGame, Feedback, Status).

% all_next_game_steps(Game, Steps) Returns all the posible steps given a Game
all_next_game_steps(Game, Steps) :-
    findall(Step, next_game_step(Game, Step), Steps).
