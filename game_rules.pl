:- module(game_rules, [queen_surrounded/2,repeated_game_positions/2]).
:- use_module(board_utils). 
:- use_module(piece_utils). 


% queen_surrounded(Board, Color) Succeed if any queen of Color is surrounded
queen_surrounded(Board, Color) :-
    queen_surrounding_pieces(Board, Color, AllSurroundingPieces),
    get_all_pieces(Board, piece(_,_,Color,[queen|_]), [Queen|_]),
    get_piece_Height(Queen, Height),
    findall(X, 
        (member(X, AllSurroundingPieces),
         piece(_, _, _, [_,Height|_]) = X
        ), SameHeightSurroundNeighbors),
    length(SameHeightSurroundNeighbors, Length),
    Length >= 6.

% repeated_game_positions(GameHistory, Amount) Return the Amount of repeated turn position assuming two players only
repeated_game_positions([], 0).
repeated_game_positions([_], 0).
repeated_game_positions([_,_], 0).
repeated_game_positions([_,_,_], 0).
repeated_game_positions([Game1,Game2,Game3,Game4|GameHistory], Amount) :- 
    game(Board1, _, _) = Game1,
    game(Board2, _, _) = Game2,
    game(Board3, _, _) = Game3,
    game(Board4, _, _) = Game4,
    (
        equal_set_board(Board1, Board3),
        equal_set_board(Board2, Board4),
        repeated_game_positions(GameHistory, ReturnedAmount),
        Amount is ReturnedAmount + 1
        ;
        Amount is 0
    ).
    
% equal_set_board(Board1, Board2) Succeed if the boards as sets are equal
equal_set_board([], []).
equal_set_board(Board1, Board2) :- 
    [Piece1|RemovedBoard1] = Board1,
    member(Piece1, Board2), !,
    remove_board_piece(Board2, Piece1, RemovedBoard2),
    equal_set_board(RemovedBoard1, RemovedBoard2).