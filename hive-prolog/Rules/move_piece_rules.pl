:- module(move_piece_rules, [move/7]).
:- use_module('../Utils/board_utils'). 
:- use_module('../Utils/list_utils'). 
:- use_module('../Utils/piece_utils'). 
:- use_module(move_utils). 
:- use_module(bug_movement). 

% Movement Rules

% top_at_position(Board, PieceToMove). Succeed if PieceToMove is at the top position height 
top_at_position(OldBoard, Piece) :-
    piece(PosX, PosY, _, _) = Piece,
    get_piece_Height(Piece, Height), 
    not((get_first_piece(OldBoard, piece(PosX, PosY, _, _), piece(PosX, PosY, _, [_, Height2|_])), Height < Height2)).

% queen_present(Board, Piece) Succeed if there is a queen of the piece color in Board. 
queen_present(Board, Piece) :- 
    get_piece_Color(Piece, Color),
    member(piece(_,_,Color,[queen|_]), Board), 
    !.
    

% connected_board_if_removed(Board, Piece) Succeed if Board is connected after removing Piece
connected_board_if_removed(Board, Piece) :- 
    remove_board_piece(Board, Piece, ResultBoard),
    connected_board(ResultBoard).

% single_hive_after(Board, PieceToRemove, PieceToAdd) Succeed if Board fulfil Unique Hive Rule 
single_hive_after(Board, PieceToRemove, PieceToAdd) :- 
    remove_board_piece(Board, PieceToRemove, ResultBoard),
    connected_board(ResultBoard),
    connected_board([PieceToAdd|ResultBoard]).

% pre_move_rules(Board, Piece) Rules that must fulfil all pieces on given Board to be able to move
pre_move_rules(Board, Piece) :- 
    top_at_position(Board, Piece),
    queen_present(Board, Piece).

% post_move_rules(OldBoard, OldPiece, NewPiece, NewBoard) Rules that must be fulfilled after the piece is moved
post_move_rules(_, _, _, NewBoard) :- 
    connected_board(NewBoard). % Single Hive Simplified
    % single_hive_after(OldBoard, OldPiece, NewPiece).

% move(Board, OldPiece, ExtraArgs, NewPosX, NewPosY, NewPiece, NewBoard) Return the NewBoard after the move is made.
move(Board, OldPiece, ExtraArgs, NewPosX, NewPosY, NewPiece, NewBoard) :- 
    get_piece_Type(OldPiece, Type),
    bug_movement_functor(Functor, Type),
    pre_move_rules(Board, OldPiece),
    Function =.. [Functor, Board, OldPiece, ExtraArgs, NewPosX, NewPosY, NewPiece, NewBoard],
    call(Function),
    post_move_rules(Board, OldPiece, NewPiece, NewBoard).
