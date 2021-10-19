:- module(rules, [add_piece/3]).

% Pieces
ant.
queen.
cricket.
spider.

% Color
white.
black.

% Piece Definition
piece(PosX, PosY, Color, [Type, Height]).
get_piece_X(piece(PosX,_,_,_), PosX).
get_piece_Y(piece(_,PosY,_,_), PosY).
get_piece_Color(piece(_,_,Color,_), Color).
get_piece_Type(piece(_,_,_,[Type|_]), Type).
get_piece_Height(piece(_,_,_,[_,Height|_]), Height).

% UTILS

% positions_next_to(Piece, PosX, PosY)
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 0 is PosY mod 2,
                                                    PosXNew is PosX-1,
                                                    PosYNew is PosY-1. % Up Left
positions_next_to(PosX, PosY, PosX, PosYNew) :- 1 is PosY mod 2,
                                                PosYNew is PosY-1. % Up Left
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 0 is PosY mod 2,
                                                    PosXNew is PosX,
                                                    PosYNew is PosY-1. % Up Right
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 1 is PosY mod 2,
                                                    PosXNew is PosX+1,
                                                    PosYNew is PosY-1. % Up Right
positions_next_to(PosX, PosY, PosXNew, PosY) :- PosXNew is PosX+1. % Right
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 1 is PosY mod 2,
                                                    PosXNew is PosX+1, 
                                                    PosYNew is PosY+1. % Down Right
positions_next_to(PosX, PosY, PosX, PosYNew) :- 0 is PosY mod 2,
                                                PosYNew is PosY+1. % Down Right
positions_next_to(PosX, PosY, PosX, PosYNew) :- 1 is PosY mod 2,
                                                PosYNew is PosY+1. % Down Left
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 0 is PosY mod 2,
                                                    PosXNew is PosX-1, 
                                                    PosYNew is PosY+1. % Down Left
positions_next_to(PosX, PosY, PosXNew, PosY) :- PosXNew is PosX-1. % Left

% pieces_together(Piece1, Piece2) 
pieces_together(piece(PosX1, PosY1, _, _), piece(PosX2, PosY2, _, _)) :- positions_next_to(PosX1, PosY1, PosX2, PosY2).

% pieces_together(Piece, Pieces, PiecesTogether)
pieces_together(piece(PosX1, PosY1, _, _), [], []) :- !.
pieces_together(Piece1, [Piece2|Pieces], [Piece2|Result]) :- pieces_together(Piece1, Piece2), 
                                                             pieces_together(Piece1, Pieces, Result), !.
pieces_together(Piece1, [Piece2|Pieces], Result) :- pieces_together(Piece1, Pieces, Result).

% placed_around_of(Board, Piece, AroundPiece): Return the pieces that are next to Piece in Board
placed_around_of([BoardPiece|Board], Piece, BoardPiece) :- pieces_together(BoardPiece, Piece).
placed_around_of([_|Board], Piece, BoardPiece) :- placed_around_of(Board, Piece, BoardPiece).

% color_played_list(Board, Color, Pieces): Returns in Pieces all the pieces of color Color
color_played_list([], _, []).
color_played_list([Piece|Pieces], Color, [Piece|Result]) :- piece(_,_,Color,_) = Piece,
                                                            color_played_list(Pieces, Color, Result), 
                                                            !.
color_played_list([_|Pieces], Color, Result) :- color_played_list(Pieces, Color, Result).

% exist_queen(Pieces): Succeed if a queen is found
exist_queen([Piece|_]) :- get_piece_Type(Piece, queen), !.
exist_queen([_|Pieces]) :- exist_queen(Pieces). 


% Rules

% Pieces can be placed only next to pieces of the same color and away from pieces of different colors
piece_next_to_allied_and_away_from_enemy(InitialBoard, Piece) :- placed_around_of(InitialBoard, Piece, NeighborPiece),
                                                  piece(_, _, C1, _) = Piece,
                                                  piece(_, _, C1, _) = NeighborPiece,
                                                  not((placed_around_of(InitialBoard, Piece, piece(_, _, C2, _)),C2\=C1)).

% place_piece_rules(Board, Piece): Succeed if all rules regarding placing a piece are true
place_piece_rules(InitialBoard, Piece) :- piece_next_to_allied_and_away_from_enemy(InitialBoard, Piece).

% Add Pieces Rules

% add_piece(Pieces, Piece, NewPieces).
% Empty Board: The Piece can be placed anywhere
add_piece([], Piece, [Piece]) :- !.

% First Play: The Piece must be from different color and 
% must be placed next to the existing piece
add_piece(InitialBoard, Piece, [Piece|InitialBoard]) :- piece(_, _, C2, _) = Piece, 
                                                 color_played_list(InitialBoard, C2, C2Pieces),
                                                 length(C2Pieces, 0),
                                                 placed_around_of(InitialBoard, Piece, _),
                                                 !.
% Between the second and third play the queen can be out of the board
add_piece(InitialBoard, Piece, [Piece|InitialBoard]) :- piece(_, _, C2, _) = Piece, 
                                            color_played_list(InitialBoard, C2, C2Pieces),
                                            length(C2Pieces, Length),
                                            0 < Length, Length < 3,
                                            place_piece_rules(InitialBoard, Piece),
                                            !.

% Forth play and ahead, queen must be on the board
add_piece(InitialBoard, Piece, [Piece|InitialBoard]) :- piece(_, _, C2, _) = Piece, 
                                        color_played_list(InitialBoard, C2, C2Pieces),
                                        length(C2Pieces, Length),
                                        Length > 2,
                                        exist_queen([Piece|C2Pieces]),
                                        place_piece_rules(InitialBoard, Piece),
                                        !.

% MovementRules

% Queen Movement
% TODO POR AQUI HACIENDO LOS MOVIMIENTOS
move_piece(OldBoard, OldPiece, NewPiece, NewBoard) :- false.
