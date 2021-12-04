:- module(console_utils, [print_board/1, print_pieces/1, read_position/2, 
            read_with_headline/2, print_game_state/3, print_game_state/1, 
            print_game_feedback/6, default_if_empty/4]).

print_board(Board) :- 
    write('Board'), nl,
    write(Board).

print_pieces([]).
print_pieces([pieces_info(ColorPlayer, PiecesToSet)|OtherPlayers]) :- 
    write(ColorPlayer), write(' pieces:'), nl,
    write(PiecesToSet), nl, nl,
    print_pieces(OtherPlayers).

read_position(PosX, PosY) :- 
    read_with_headline('Position X', PosX), 
    read_with_headline('Position Y', PosY). 

read_with_headline(Headline, Read) :- 
    write(Headline), nl,
    read(Read).

default_if_empty(Value, Value, Default, Default) :- !.
default_if_empty(Value, Value2, _, Value) :- Value \= Value2.

% print_game_state(Game, PlayerColor, ExtraGameConfig)
print_game_state(Game, _, _) :- 
    print_game_state(Game).

print_game_state(Game) :- 
    game(Board, CurrentPlayer, [PlayerPieces|_]) = Game,
    print_board(Board), nl,
    print_pieces(PlayerPieces), nl,
    write('Player '), write(CurrentPlayer), write(' turn:'), nl.

% print_game_feedback(PlayerColor, Action, NewGame, Feedback, Status, ExtraGameConfig) :-
print_game_feedback(_, Action, _, Feedback, Status, _) :-
    write(Action),nl,
    write(Status),nl,
    write(Feedback),nl.
