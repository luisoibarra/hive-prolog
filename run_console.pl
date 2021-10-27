:- module(run_console,[init_game/0,run_game/1]).
:- use_module(run_game_utils). 
% Console Interface

print_board(Board) :- write(Board).

print_pieces(BlackTypePieces, WhiteTypePieces) :- 
    write(white), nl,
    write(WhiteTypePieces), nl, nl,
    write(black), nl,
    write(BlackTypePieces), nl, nl.

read_position(PosX, PosY) :- 
    read_with_headline('Position X', PosX), 
    read_with_headline('Position Y', PosY). 

read_with_headline(Headline, Read) :- 
    write(Headline), nl,
    read(Read).

init_game() :-
    game([],white,[
        [queen, cricket, cricket, cricket, ant, ant, ant, beetle, beetle, spider, spider],
        [queen, cricket, cricket, cricket, ant, ant, ant, beetle, beetle, spider, spider]
    ]) = Game,
    run_game(Game).

run_game(Game) :- 
    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces|_]) = Game,
    write('Player '), write(CurrentPlayer), write(' turn'), nl,
    print_board(Board), nl,
    print_pieces(BlackTypePieces, WhiteTypePieces), nl,
    write('Player '), write(CurrentPlayer), write(' turn:'), nl,
    write('1: For place a piece'), nl, 
    write('2: For move a piece'), nl,
    read_with_headline('', Option),
    (
        1 = Option,
        read_with_headline('Select piece to place:', PieceTypeAdd),
        write('Write the position'), nl,
        read_position(PosX, PosY),
        set_piece(PieceTypeAdd, PosX, PosY, Game, NewGame)
        ;
        2 = Option,
        write('Write the position of the piece to move'), nl,
        read_position(PosX, PosY),
        write('Write the detination position'), nl,
        read_position(DestPosX, DestPosY),
        move_piece(PosX, PosY, DestPosX, DestPosY, Game, NewGame)
    ),
    run_game(NewGame).