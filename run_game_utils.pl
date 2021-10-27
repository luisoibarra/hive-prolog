:- module(run_game_utils,[set_piece/5, move_piece/6, end_rotation_feedback/3, get_game_GameHistory/2]).
:- use_module(add_piece_rules). 
:- use_module(game_rules). 
:- use_module(move_piece_rules). 
:- use_module(board_utils). 
:- use_module(list_utils). 
:- use_module(piece_utils). 


% game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces, GameHistory]).
get_game_GameHistory(Game, GameHistory) :- 
    game(_, _, [_,_, GameHistory|_]) = Game.


switch_player(white, black). 
switch_player(black, white). 

% get_piece_type_to_play(CurrentPlayer, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, NewWhiteTypePieces, NewBlackTypePieces) 
get_piece_type_to_play(white, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, NewWhiteTypePieces, BlackTypePieces) :- 
    element_at(WhiteTypePieces, PiecePosition, PieceType),
    remove_at(WhiteTypePieces, PiecePosition, NewWhiteTypePieces).
get_piece_type_to_play(black, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, WhiteTypePieces, NewBlackTypePieces) :- 
    element_at(BlackTypePieces, PiecePosition, PieceType),
    remove_at(BlackTypePieces, PiecePosition, NewBlackTypePieces).

% set_piece(PiecePosition, PosX, PosY, Game, NewGame)
set_piece(PiecePosition, PosX, PosY, Game, NewGame) :- 
    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces, GameHistory|Extra]) = Game,
    
    get_piece_type_to_play(CurrentPlayer, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, NewWhiteTypePieces, NewBlackTypePieces),
    build_piece(PosX, PosY, CurrentPlayer, [PieceType, 0], Piece),
    add_piece(Board, Piece, NewBoard),

    switch_player(CurrentPlayer, NewCurrentPlayer),
    game(NewBoard, NewCurrentPlayer, [NewWhiteTypePieces, NewBlackTypePieces, [Game|GameHistory]|Extra]) = NewGame.

% move_piece(PosX, PosY, DestPosX, DestPosY, Game, NewGame) 
move_piece(PosX, PosY, DestPosX, DestPosY, Game, NewGame) :- 
    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces, GameHistory|Extra]) = Game,
    get_top_piece_at(Board, PosX, PosY, PieceToMove),
    get_piece_Type(PieceToMove, CurrentPlayer),

    move(Board, PieceToMove, DestPosX, DestPosY, _, NewBoard),

    switch_player(CurrentPlayer, NewCurrentPlayer),
    game(NewBoard, NewCurrentPlayer, [WhiteTypePieces, BlackTypePieces, [Game|GameHistory]|Extra]) = NewGame.

end_rotation_feedback(Game, Feedback, ContinueGame) :-
    nonvar(ContinueGame),

    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces, GameHistory|Extra]) = Game,
    switch_player(PrevPlayer, CurrentPlayer),
    repeated_game_positions(GameHistory, Amount),
    (
        queen_surrounded(Board, PrevPlayer),
        string_concat('Game Over ', PrevPlayer, Feedback),
        ContinueGame = false
        ;
        Amount >= 3,
        Feedback = 'Game Over, repeated positions detected. Tie',
        ContinueGame = false
        ; 
        Feedback = '',
        ContinueGame = true
    ).
