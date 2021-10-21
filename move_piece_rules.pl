:- module(move_piece_rules, [move/4]).
:- use_module(board_utils). 
:- use_module(list_utils). 
:- use_module(piece_utils). 

% Movement Rules

% top_at_position(Board, PieceToMove). Succeed if PieceToMove it is at the top position height 
top_at_position(OldBoard, piece(PosX, PosY, _, [_,Height|_])) :- 
    not((get_all_pieces(OldBoard, piece(PosX, PosY, _, _), piece(PosX, PosY, _, [_, Height2|_])), Height < Height2)).

% connected_board_if_removed(Board, Piece) Succeed if Board is connected after removing Piece
connected_board_if_removed(Board, Piece) :- 
    remove_board_piece(Board, Piece, ResultBoard),
    connected_board(ResultBoard).

% single_hive_after(Board, PieceToRemove, PieceToAdd) Succeed if Board fulfil Unique Hive Rule 
single_hive_after(Board, PieceToRemove, PieceToAdd) :- 
    remove_board_piece(Board, PieceToRemove, ResultBoard),
    connected_board(ResultBoard),
    connected_board([PieceToAdd|ResultBoard]).

% Queen Movement
queen_moves_position(Board, Piece, NewPiece) :- 
    piece(PosX, PosY,Color,Extra) = Piece,
    positions_next_to(PosX, PosY, NewPosX, NewPosY),
    piece(NewPosX, NewPosY, Color, Extra) = NewPiece,
    can_slide_into(Board, PosX, PosY, NewPosX, NewPosY),
    single_hive_after(Board, Piece, NewPiece),
    not(is_place_taken(Board, NewPosX, NewPosY)).

% move(Board, OldPiece, NewPiece, NewBoard) Return the NewBoard after the move is made.
move(Board, OldPiece, NewPiece, [NewPiece|RemovedBoard]) :- 
    get_piece_Type(OldPiece, queen),
    queen_moves_position(Board, OldPiece, NewPiece),
    remove_board_piece(Board, OldPiece, RemovedBoard).
