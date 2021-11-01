:- module(players, [random_player/2, console_human_player/2]).
:- use_module(list_utils). 
:- use_module(piece_utils). 
:- use_module(board_utils). 
:- use_module(move_utils). 
:- use_module(run_game_utils).
:- use_module(console_utils).

% next_game_step(Game, Action, NewGame, Feedback, Status) Given a Game returns a posible Action NewGame Feedback Status
next_game_step(Game, Action, NewGame, Feedback, Status) :-
    make_a_play(Action, Game, NewGame, Feedback, Status).

% all_next_game_steps(Game, Steps) Returns all the posible steps given a Game
all_next_game_steps(Game, Steps) :-
    findall(step(A,NG,Fs,Sts), next_game_step(Game, A, NG, Fs, Sts), Steps).

% game([],white,[[queen,ant,ant],[queen,ant,ant],[],1])
% game([piece(3,3,white,[queen,0])],black,[[queen,ant,ant],[queen,ant,ant],[],1])

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