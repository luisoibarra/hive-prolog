:- module(board_utils, 
    [remove_board_piece/3, is_place_taken/3, connected_board/1, positions_next_to/4,
    pieces_together/3, can_slide_into/5, get_all_pieces/3, exist_queen/1, color_played_list/3,
    placed_around_of/3
    ]).
:- use_module(list_utils). 
:- use_module(piece_utils). 


% remove_board_piece(Board, PieceToRemove, ResultBoard). Succeed if Piece is found in Board returning the ResultBoard without the Piece
remove_board_piece([Piece|Board], Piece, Board) :- !.
remove_board_piece([OtherPiece|Board], Piece, [OtherPiece|ResultBoard]) :- remove_board_piece(Board, Piece, ResultBoard).

% is_place_taken(Board, PosX, PosY) Succeed if there is a piece at PosX PosY
is_place_taken([piece(PosX,PosY,_,_)|_], PosX, PosY) :- !.
is_place_taken([_|Board], PosX, PosY) :- is_place_taken(Board, PosX, PosY).



% connected_board(Board) Succeed if Board is connected
connected_board([]).
connected_board([Piece|Board]) :- connected_board([Piece|Board], [Piece], [Piece]).

connected_board(Board, [], Visited) :- length(Board, LengthB),
                                       length(Visited, LengthB),
                                       !.
connected_board(Board, [Piece|ToProcess], Visited) :- 
    pieces_together(Piece, Board, Neighbors),
    list_difference(Neighbors, Visited, NotVisitedNeigbors),
    concat_list(NotVisitedNeigbors, ToProcess, NewToProcess),
    first_element_list_or_empty_list(NewToProcess, NewVisitedItem),
    concat_list(NewVisitedItem, Visited, NewVisited),
    connected_board(Board, NewToProcess, NewVisited).



% positions_next_to(Piece, PosX, PosY)
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 0 is PosX mod 2,
                                                    PosXNew is PosX-1,
                                                    PosYNew is PosY-1. % Up Left
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 1 is PosX mod 2,
                                                   PosXNew is PosX-1, % Up Left
                                                   PosYNew is PosY.
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- PosYNew is PosY-1, % Up
                                                   PosXNew is PosX.
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 0 is PosX mod 2,
                                                    PosXNew is PosX+1,
                                                    PosYNew is PosY-1. % Up Right
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 1 is PosX mod 2,
                                                    PosXNew is PosX+1,
                                                    PosYNew is PosY. % Up Right
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 1 is PosX mod 2,
                                                    PosXNew is PosX+1, 
                                                    PosYNew is PosY+1. % Down Right
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 0 is PosX mod 2,
                                                PosXNew is PosX+1, 
                                                PosYNew is PosY. % Down Right
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- PosYNew is PosY+1, % Down
                                                   PosXNew is PosX.
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 1 is PosX mod 2,
                                                   PosXNew is PosX-1, 
                                                   PosYNew is PosY+1. % Down Left
positions_next_to(PosX, PosY, PosXNew, PosYNew) :- 0 is PosX mod 2,
                                                    PosXNew is PosX-1, 
                                                    PosYNew is PosY. % Down Left



% pieces_together(Piece1, Piece2) 
pieces_together(piece(PosX1, PosY1, _, _), piece(PosX2, PosY2, _, _)) :- positions_next_to(PosX1, PosY1, PosX2, PosY2).

% pieces_together(Piece, Board, PiecesTogether)
pieces_together(piece(_, _, _, _), [], []) :- !.
pieces_together(Piece1, [Piece2|Pieces], [Piece2|Result]) :- pieces_together(Piece1, Piece2), 
                                                             pieces_together(Piece1, Pieces, Result), !.
pieces_together(Piece1, [_|Pieces], Result) :- pieces_together(Piece1, Pieces, Result).

% placed_around_of(Board, Piece, AroundPiece): Return the pieces that are next to Piece in Board
placed_around_of([BoardPiece|_], Piece, BoardPiece) :- pieces_together(BoardPiece, Piece).
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

% get_all_pieces(Pieces, Pattern, Result): Succeed if a piece in Pieces unifies with Pattern
get_all_pieces([First|_], PiecePatter, First) :- PiecePatter = First.
get_all_pieces([_|Pieces], PiecePatter, Result) :- get_all_pieces(Pieces, PiecePatter, Result).



% can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) Succeed if a piece standing in PosX and PosY can slide into NewPosX and NewPosY
can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to UpLeft
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 1,
    DirY = 1,
    0 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY-1,
    PosX2 is PosX-1, PosY2 is PosY,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).

can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to UpLeft
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 1,
    DirY = 0,
    1 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY-1,
    PosX2 is PosX-1, PosY2 is PosY+1,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).

can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Up
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 0,
    DirY = 1,
    0 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY-1,
    PosX2 is PosX+1, PosY2 is PosY-1,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).

can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Up
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 0,
    DirY = 1,
    1 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY,
    PosX2 is PosX+1, PosY2 is PosY,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).

can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Up Right
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = -1,
    DirY = 1,
    0 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY-1,
    PosX2 is PosX+1, PosY2 is PosY,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).

can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Up Right
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = -1,
    DirY = 0,
    1 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY-1,
    PosX2 is PosX+1, PosY2 is PosY+1,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).

can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Down Right
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = -1,
    DirY = 0,
    0 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY+1,
    PosX2 is PosX+1, PosY2 is PosY-1,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).
can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Down Right
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = -1,
    DirY = -1,
    1 is PosX mod 2,
    PosX1 is PosX+1, PosY1 is PosY,
    PosX2 is PosX, PosY2 is PosY+1,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).

can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Down
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 0,
    DirY = -1,
    0 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY,
    PosX2 is PosX+1, PosY2 is PosY,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).
can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Down
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 0,
    DirY = -1,
    1 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY+1,
    PosX2 is PosX+1, PosY2 is PosY+1,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).

can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Down Left
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 1,
    DirY = 0,
    0 is PosX mod 2,
    PosX1 is PosX+1, PosY1 is PosY+1,
    PosX2 is PosX, PosY2 is PosY+1,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).

can_slide_into(Board, PosX, PosY, NewPosX, NewPosY) :- % Slide to Down Left
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 1,
    DirY = -1,
    1 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY,
    PosX2 is PosX, PosY2 is PosY+1,
    not((is_place_taken(Board, PosX1, PosY1), is_place_taken(Board, PosX2, PosY2))).
