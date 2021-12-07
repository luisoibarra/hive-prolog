:- module(add_piece_rules, [add_piece/3]).
:- use_module('../Utils/board_utils'). 
:- use_module('../Utils/list_utils'). 
:- use_module('../Utils/piece_utils'). 


% Rules

% Pieces can be placed only next to pieces of the same color and away from pieces of different colors
piece_next_to_allied_and_away_from_enemy(InitialBoard, Piece) :- 
    placed_around_of(InitialBoard, Piece, NeighborPiece),
    piece(PosX, PosY, C1, _) = Piece,
    piece(NeighPosX, NeighPosY, C1, _) = NeighborPiece,
    get_top_piece_at(InitialBoard, NeighPosX, NeighPosY, NeighborPiece),
    not(is_place_taken(InitialBoard, PosX, PosY, 0)),
    not((
        placed_around_of(InitialBoard, Piece, NewNeighborPiece),
        piece(NewPosX, NewPosY, C2, _) = NewNeighborPiece,
        C2\=C1,
        get_top_piece_at(InitialBoard, NewPosX, NewPosY, NewNeighborPiece))
    ).

% place_piece_rules(Board, Piece): Succeed if all rules regarding placing a piece are true
place_piece_rules(InitialBoard, Piece) :- piece_next_to_allied_and_away_from_enemy(InitialBoard, Piece).

% Add Pieces Rules

% add_piece(Pieces, Piece, NewPieces).
% Empty Board: The Piece can be placed anywhere
add_piece([], Piece, [Piece]) :- !.

% First Play: The Piece must be from different color and 
% must be placed next to the existing piece
add_piece(InitialBoard, Piece, [Piece|InitialBoard]) :- piece(PosX, PosY, C2, _) = Piece, 
                                                 color_played_list(InitialBoard, C2, C2Pieces),
                                                 length(C2Pieces, 0),
                                                 placed_around_of(InitialBoard, Piece, _),
                                                 not(is_place_taken(InitialBoard, PosX, PosY, 0)).
% Between the second and third play the queen can be out of the board
add_piece(InitialBoard, Piece, [Piece|InitialBoard]) :- piece(_, _, C2, _) = Piece, 
                                            color_played_list(InitialBoard, C2, C2Pieces),
                                            length(C2Pieces, Length),
                                            0 < Length, Length < 3,
                                            place_piece_rules(InitialBoard, Piece).

% Forth play and ahead, queen must be on the board
add_piece(InitialBoard, Piece, [Piece|InitialBoard]) :- piece(_, _, C2, _) = Piece, 
                                        color_played_list(InitialBoard, C2, C2Pieces),
                                        length(C2Pieces, Length),
                                        Length > 2,
                                        exist_queen([Piece|C2Pieces]),
                                        place_piece_rules(InitialBoard, Piece).
