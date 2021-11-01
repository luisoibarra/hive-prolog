:- module(console_utils, [print_board/1, print_pieces/2, read_position/2, 
            read_with_headline/2, print_game_state/1, print_game_feedback/4]).

print_board(Board) :- 
    write('Board'), nl,
    write(Board).

print_pieces(BlackTypePieces, WhiteTypePieces) :- 
    write(white), write(' pieces'), nl,
    write(WhiteTypePieces), nl, nl,
    write(black), write(' pieces'), nl,
    write(BlackTypePieces), nl.

read_position(PosX, PosY) :- 
    read_with_headline('Position X', PosX), 
    read_with_headline('Position Y', PosY). 

read_with_headline(Headline, Read) :- 
    write(Headline), nl,
    read(Read).

print_game_state(Game) :- 
    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces|_]) = Game,
    write('Player '), write(CurrentPlayer), write(' turn'), nl,
    print_board(Board), nl,
    print_pieces(BlackTypePieces, WhiteTypePieces), nl,
    write('Player '), write(CurrentPlayer), write(' turn:'), nl.

% print_game_feedback(Action, NewGame, Feedback, Status) :-
print_game_feedback(_, _, Feedback, _) :-
    write(Feedback),nl.
