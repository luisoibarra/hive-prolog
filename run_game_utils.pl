:- module(run_game_utils,[set_piece/5, move_piece/6]).
:- use_module(add_piece_rules). 
:- use_module(move_piece_rules). 
:- use_module(board_utils). 
:- use_module(list_utils). 
:- use_module(piece_utils). 


% game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces]).

switch_player(white, black). 
switch_player(balck, white). 

% get_piece_type_to_play(CurrentPlayer, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, NewWhiteTypePieces, NewBlackTypePieces) 
get_piece_type_to_play(white, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, NewWhiteTypePieces, BlackTypePieces) :- 
    element_at(WhiteTypePieces, PiecePosition, PieceType),
    remove_at(WhiteTypePieces, PiecePosition, NewWhiteTypePieces).
get_piece_type_to_play(black, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, WhiteTypePieces, NewBlackTypePieces) :- 
    element_at(BlackTypePieces, PiecePosition, PieceType),
    remove_at(BlackTypePieces, PiecePosition, NewBlackTypePieces).

% set_piece(PiecePosition, PosX, PosY, Game, NewGame)
set_piece(PiecePosition, PosX, PosY, Game, NewGame) :- 
    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces|Extra]) = Game,
    
    get_piece_type_to_play(CurrentPlayer, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, NewWhiteTypePieces, NewBlackTypePieces),
    build_piece(PosX, PosY, CurrentPlayer, [PieceType, 0], Piece),
    add_piece(Board, Piece, NewBoard),

    switch_player(CurrentPlayer, NewCurrentPlayer),
    game(NewBoard, NewCurrentPlayer, [NewWhiteTypePieces, NewBlackTypePieces|Extra]) = NewGame.

% move_piece(PosX, PosY, DestPosX, DestPosY, Game, NewGame) 
move_piece(PosX, PosY, DestPosX, DestPosY, Game, NewGame) :- 
    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces|Extra]) = Game,
    get_top_piece_at(Board, PosX, PosY, PieceToMove),
    get_piece_Type(PieceToMove, CurrentPlayer),

    move(Board, PieceToMove, DestPosX, DestPosY, _, NewBoard),

    switch_player(CurrentPlayer, NewCurrentPlayer),
    game(NewBoard, NewCurrentPlayer, [WhiteTypePieces, BlackTypePieces|Extra]) = NewGame.


