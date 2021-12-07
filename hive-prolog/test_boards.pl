:- use_module(add_piece_rules). 
:- use_module(move_piece_rules). 


% test_boards(InitialBoard, PiecesToBeAdded, ExpectedResult)
% Empty Board
test_boards([],[],true) :- write(0). 

% First Play Board
test_boards([piece(0,0,white,_)], [piece(0,1,black,_)], true) :- write(1).
test_boards([piece(0,0,_,_)], [piece(1,1,_,_)], false) :- write(2).

% Add piece next to allied and away from enemies
test_boards([piece(2,2,white,_)], [piece(2,3,black,_), piece(3,2,white,_)], false) :- write(3).
test_boards([piece(2,2,white,_)], [piece(2,3,black,_), piece(1,1,white,_)], true) :- write(4).

% Queen must be setted before the 4th play
test_boards([piece(2,2,white,[ant|_])], [piece(2,3,black,[ant|_]), piece(1,1,white,[ant|_]), piece(2,4,black,[ant|_]), piece(1,0,white,[ant|_]), piece(2,5,black,[ant|_]), piece(1,1,white,[queen|_])], true) :- write(5).
test_boards([piece(2,2,white,[ant|_])], [piece(2,3,black,[ant|_]), piece(1,1,white,[ant|_]), piece(2,4,black,[ant|_]), piece(1,0,white,[ant|_]), piece(2,5,black,[ant|_]), piece(1,1,white,[ant|_])], false) :- write(6).

% Queen Movement
test_boards([piece(2,2,white,[queen,0|_]),piece(1,1,_,[_,0|_]),piece(1,2,_,[_,0|_])], [[piece(2,2,white,[queen,0|_]),[2,1]]], true) :- write(7).
test_boards([piece(2,2,white,[queen,0|_]),piece(1,1,_,[_,0|_]),piece(2,1,_,[_,0|_])], [[piece(2,2,white,[queen,0|_]),[2,1]]], false) :- write(8).

% Cricket Movement
test_boards([piece(2,2,_,[cricket,0|_]),piece(3,2,_,[_,0|_]),piece(4,3,_,[_,0|_])], [[piece(2,2,_,[cricket,0|_]),[1,2]]], false) :- write(9).
test_boards([piece(2,2,_,[cricket,0|_]),piece(3,2,_,[_,0|_]),piece(4,3,_,[_,0|_])], [[piece(2,2,_,[cricket,0|_]),[4,3]]], false) :- write(10).
test_boards([piece(2,2,_,[cricket,0|_]),piece(3,2,_,[_,0|_]),piece(4,3,_,[_,0|_])], [[piece(2,2,_,[cricket,0|_]),[4,2]]], false) :- write(11).
test_boards([piece(2,2,_,[cricket,0|_]),piece(3,2,_,[_,0|_]),piece(4,3,_,[_,0|_])], [[piece(2,2,_,[cricket,0|_]),[5,3]]], true) :- write(12).

% Beetle Movement
test_boards([piece(2,2,_,[queen, 0|_]),piece(3,2,_,[beetle, 0|_]),piece(2,3,_,[beetle, 0|_]),piece(4,3,_,[beetle, 0|_]),piece(4,2,_,[beetle, 0|_]),piece(3,1,_,[beetle, 0|_])], [[piece(2,3,_,[beetle, 0|_]),[3,2]]], true) :- write(13).
test_boards([piece(2,2,_,[queen, 0|_]),piece(3,2,_,[beetle, 0|_]),piece(2,3,_,[beetle, 0|_]),piece(4,3,_,[beetle, 0|_]),piece(4,2,_,[beetle, 0|_]),piece(3,1,_,[beetle, 0|_]),piece(3,3,_,[beetle, 0|_])], [[piece(2,3,_,[beetle, 0|_]),[3,2]]], true) :- write(14).
test_boards([piece(2,2,_,[queen, 0|_]),piece(3,2,_,[beetle, 0|_]),piece(2,3,_,[beetle, 0|_]),piece(3,3,_,[beetle, 1|_]),piece(3,3,_,[beetle, 0|_]),piece(2,2,_,[beetle, 1|_]),piece(4,3,_,[beetle, 0|_]),piece(4,2,_,[beetle, 0|_]),piece(3,1,_,[beetle, 0|_])], [[piece(2,3,_,[beetle, 0|_]),[3,2]]], false) :- write(15).
test_boards([piece(2,2,_,[queen, 0|_]),piece(3,2,_,[beetle, 0|_]),piece(3,2,_,[beetle, 1|_]),piece(3,3,_,[beetle, 1|_]),piece(3,3,_,[beetle, 0|_]),piece(2,3,_,[beetle, 0|_]),piece(2,2,_,[beetle, 1|_]),piece(3,2,_,[ant, 0|_]),piece(4,3,_,[beetle, 0|_]),piece(4,2,_,[beetle, 0|_]),piece(3,1,_,[beetle, 0|_])], [[piece(3,2,_,[beetle, 1|_]),[2,3]]], false) :- write(16).
test_boards([piece(2,2,_,[queen, 0|_]),piece(3,2,_,[beetle, 0|_]),piece(3,2,_,[beetle, 1|_]),piece(3,3,_,[beetle, 1|_]),piece(3,3,_,[beetle, 0|_]),piece(2,2,_,[beetle, 1|_]),piece(3,2,_,[ant, 0|_]),piece(4,3,_,[beetle, 0|_]),piece(4,2,_,[beetle, 0|_]),piece(3,1,_,[beetle, 0|_])], [[piece(3,2,_,[beetle, 1|_]),[2,3]]], false) :- write(17).
test_boards([piece(2,2,_,[queen, 0|_]),piece(2,2,_,[beetle, 1|_]),piece(3,2,_,[beetle, 0|_]),piece(3,2,_,[beetle, 1|_]),piece(4,3,_,[beetle, 0|_]),piece(4,2,_,[beetle, 0|_]),piece(3,1,_,[beetle, 0|_])], [[piece(3,2,_,[beetle, 1|_]),[2,3]]], true) :- write(18).

% Spider Movement
test_boards([piece(2,2,_,[queen ,0]),piece(1,2,_,[queen,0]),
             piece(0,2,_,[queen ,0]),piece(0,1,_,[queen,0]),
             piece(1,0,_,[queen ,0]),piece(2,1,_,[queen,0]),
             piece(1,1,_,[spider,0])], 
             [[piece(1,1,_,[spider,0|_]), [_,_]]], false) :- write(19).
test_boards([piece(1,2,_,[queen,0]),
             piece(0,2,_,[queen ,0]),piece(0,1,_,[queen,0]),
             piece(1,0,_,[queen ,0]),piece(2,1,_,[queen,0]),
             piece(1,1,_,[spider,0])], 
             [[piece(1,1,_,[spider,0|_]), [1,3]]], false) :- write(20).
test_boards([piece(1,2,_,[queen,0]),
            piece(0,2,_,[queen ,0]),piece(0,1,_,[queen,0]),
            piece(1,0,_,[queen ,0]),
            piece(1,1,_,[spider,0])], 
            [[piece(1,1,_,[spider,0|_]), [1,3]]], true) :- write(21).
test_boards([piece(1,2,_,[queen,0]),
            piece(0,2,_,[queen ,0]),piece(0,1,_,[queen,0]),
            piece(1,0,_,[queen ,0]),
            piece(1,1,_,[spider,0])], 
            [[piece(1,1,_,[spider,0|_]), [2,3]]], false) :- write(22).

% Ant Movement
test_boards([piece(1,2,_,[queen,0]),
            piece(0,2,_,[queen ,0]),piece(0,1,_,[queen,0]),
            piece(1,0,_,[queen ,0]),
            piece(1,1,_,[ant,0])], 
            [[piece(1,1,_,[ant,0|_]), [2,3]]], true) :- write(23).
test_boards([piece(1,2,_,[queen,0]),
            piece(0,2,_,[queen ,0]),piece(0,1,_,[queen,0]),
            piece(1,0,_,[queen ,0]),
            piece(1,1,_,[ant,0])], 
            [[piece(1,1,_,[ant,0|_]), [0,0]]], true) :- write(24).
test_boards([piece(1,2,_,[queen,0]),
            piece(0,2,_,[queen ,0]),piece(0,1,_,[queen,0]),
            piece(1,0,_,[queen ,0]),
            piece(1,1,_,[ant,0])], 
            [[piece(1,1,_,[ant,0|_]), [3,2]]], false) :- write(25).
test_boards([piece(1,2,_,[queen,0]),
            piece(0,2,_,[queen ,0]),piece(0,1,_,[queen,0]),
            piece(1,0,_,[queen ,0]),piece(2,2,_,[queen,0]),
            piece(1,1,_,[ant,0])], 
            [[piece(1,1,_,[ant,0|_]), [_,_]]], false) :- write(26).

% Mosquito Movement
test_boards([piece(4,3,_,[queen,0]), piece(4,2,_,[mosquito,0])],
            [[piece(4,2,_,[mosquito,0]), [5,2]]], true) :- write(27).

% General Movement
test_boards([piece(1,2,_,[ant,0]),
            piece(0,2,_,[ant ,0]),piece(0,1,_,[ant,0]),
            piece(1,0,_,[ant ,0]),
            piece(1,1,_,[ant,0])], 
            [[piece(1,1,_,[ant,0|_]), [2,3]]], false) :- write(27).

% Simulate game
% End Simulation
simulate_test_boards(_, [], ExpectedResult, true) :- ExpectedResult == true.
simulate_test_boards(_, [], ExpectedResult, false) :- ExpectedResult \= true.

% Move Piece Simulation
simulate_test_boards(InitialBoard, [[PieceToMove, [NewPosX, NewPosY]]|_], ExpectedResult, true) :- 
    false == ExpectedResult,
    not(move(InitialBoard, PieceToMove, NewPosX, NewPosY, _, _)), !.
simulate_test_boards(InitialBoard, [[PieceToMove, [NewPosX, NewPosY]]|_], ExpectedResult, false) :- 
    false \= ExpectedResult, 
    not(move(InitialBoard, PieceToMove, NewPosX, NewPosY, _, _)), !.
simulate_test_boards(InitialBoard, [[PieceToMove, [NewPosX, NewPosY]]|Pieces], ExpectedResult, Result) :- 
    move(InitialBoard, PieceToMove, NewPosX, NewPosY, _, NewBoard),
    !, simulate_test_boards(NewBoard, Pieces, ExpectedResult, Result).

% Add Piece Simulation
simulate_test_boards(InitialBoard, [Piece|_], ExpectedResult, true) :- not(add_piece(InitialBoard, Piece, _)),
                                                                         false == ExpectedResult, !.
simulate_test_boards(InitialBoard, [Piece|_], ExpectedResult, false) :- not(add_piece(InitialBoard, Piece, _)),
                                                                        false \= ExpectedResult, !.
simulate_test_boards(InitialBoard, [Piece|Pieces], ExpectedResult, Result) :- add_piece(InitialBoard, Piece, NewBoard),
                                                                              simulate_test_boards(NewBoard, Pieces, ExpectedResult, Result).
% Main Test Function
run_board_test() :- test_boards(InitialBoard, Pieces, ExpectedResult),
                    simulate_test_boards(InitialBoard, Pieces, ExpectedResult, Result),
                    nl, write(succeed), write(Result).
