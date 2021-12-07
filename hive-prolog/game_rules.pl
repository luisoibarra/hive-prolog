:- module(game_rules, [queen_surrounded/2,repeated_game_positions/2]).
:- use_module(board_utils). 
:- use_module(piece_utils). 

max_repetitions_allowed(3).

% queen_surrounded(Board, Color) Succeed if any queen of Color is surrounded
queen_surrounded(Board, Color) :-
    queen_surrounding_pieces(Board, Color, AllSurroundingPieces),
    get_first_piece(Board, piece(_,_,Color,[queen|_]), Queen),
    get_piece_Height(Queen, Height),
    findall(X, 
        (member(X, AllSurroundingPieces),
         piece(_, _, _, [_,Height|_]) = X
        ), SameHeightSurroundNeighbors),
    length(SameHeightSurroundNeighbors, Length),
    Length >= 6.

% repeated_game_positions(GameHistory, Amount) Return the Amount of repeated turn position assuming two players only
repeated_game_positions([], 0).
repeated_game_positions([Game|GameHistory], Amount) :-
    max_repetitions_allowed(MaxRep),
    equal_set_games(Game, GameHistory, MaxRep, Amount).

% equal_set_boards(Game, Games, MaxAmount, Amount) Returns the Amount of equal Board in Boards up to MaxAmount
equal_set_games(Game, Games, MaxAmount, Amount) :-
    equal_set_games(Game, Games, 0, MaxAmount, Amount).

equal_set_games(_, [], CurrentAmount, _, CurrentAmount).
equal_set_games(Game, [HistoryGame|Games], CurrentAmount, MaxAmount, Amount) :-
    CurrentAmount >= MaxAmount,
    Amount = CurrentAmount,
    !
    ;
    (
        game(CurrentBoard, CurrentPlayer, _) = Game,
        game(HistoryBoard, HistoryPlayer, _) = HistoryGame,
        (
            CurrentPlayer = HistoryPlayer,
            (
                equal_set_board(CurrentBoard, HistoryBoard),
                NewCurrentAmount is CurrentAmount + 1,
                !
                ;
                NewCurrentAmount is CurrentAmount,
                !
            ),
            !
            ;
            CurrentPlayer \= HistoryPlayer,
            NewCurrentAmount is CurrentAmount,
            !
        ),
        equal_set_games(Game, Games, NewCurrentAmount, MaxAmount, Amount)
    ).

% equal_set_board(Board1, Board2) Succeed if the boards as sets are equal
equal_set_board([], []).
equal_set_board(Board1, Board2) :- 
    [Piece1|RemovedBoard1] = Board1,
    member(Piece1, Board2), !,
    remove_board_piece(Board2, Piece1, RemovedBoard2),
    equal_set_board(RemovedBoard1, RemovedBoard2).