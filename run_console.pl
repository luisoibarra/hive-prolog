:- module(run_console,[init_game/0,run_game/1]).
:- use_module(run_game_utils). 
% Console Interface

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

init_game() :-
    game([],white,[
        [queen, cricket, cricket, cricket, ant, ant, ant, beetle, beetle, spider, spider],
        [queen, cricket, cricket, cricket, ant, ant, ant, beetle, beetle, spider, spider],
        [],
        1
    ]) = Game,
    run_game(Game).

run_game(Game) :- 
    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces, Turn|_]) = Game,
    write('Player '), write(CurrentPlayer), write(' turn'), nl,
    print_board(Board), nl,
    print_pieces(BlackTypePieces, WhiteTypePieces), nl,
    write('Player '), write(CurrentPlayer), write(' turn:'), nl,
    write('1: For place a piece'), nl, 
    write('2: For move a piece'), nl,
    read_with_headline('', Option),
    (
        1 = Option,
        read_with_headline('Select piece to place:', PositionPieceTypeAdd),
        write('Write the position'), nl,
        read_position(PosX, PosY),
        make_a_play(set_play(PositionPieceTypeAdd, PosX, PosY), Game, NewGame, Feedback, GameStatus)
        ;
        2 = Option,
        write('Write the position of the piece to move'), nl,
        read_position(PosX, PosY),
        write('Write the detination position'), nl,
        read_position(DestPosX, DestPosY),
        make_a_play(move_play(PosX, PosY,DestPosX, DestPosY), Game, NewGame, Feedback, GameStatus)
    ),
    nl,!,
    write(Feedback),nl,
    GameStatus = continue,
    run_game(NewGame).

run_game(Game) :- 
    write('Some error or invalid play was made, please verify'), nl,
    run_game(Game).