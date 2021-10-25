:- module(board_utils, 
    [remove_board_piece/3, is_place_taken/4, connected_board/1, positions_next_to/5,
    pieces_together/3, can_slide_into/5, get_all_pieces/3, exist_queen/1, color_played_list/3,
    placed_around_of/3, get_all_pieces_list/3, get_all_pieces_at/4
    ]).
:- use_module(list_utils). 
:- use_module(piece_utils). 


% get_position_max_Height_or_0(Board, PosX, PosY, MaxHeight): Returns the MaxHeight of all pieces at PosX, PosY in Board
get_position_max_Height_or_0(Board, PosX, PosY, MaxHeight) :- 
    get_all_pieces_at(Board, PosX, PosY, PosPieces),
    get_position_max_Height_or_0(PosPieces, 0, MaxHeight).

% get_position_max_Height_or_0(PositionPieces, CurrentMax, MaxHeight): Returns in the maximun height of all PositionPieces 
get_position_max_Height_or_0([], Current, Current).
get_position_max_Height_or_0([Piece|Rest], CurrentMax, MaxHeight) :- 
    get_piece_Height(Piece, Height),
    Height > CurrentMax,
    get_position_max_Height_or_0(Rest, Height, MaxHeight).
get_position_max_Height_or_0([Piece|Rest], CurrentMax, MaxHeight) :- 
    get_piece_Height(Piece, Height),
    not(Height > CurrentMax),
    get_position_max_Height_or_0(Rest, CurrentMax, MaxHeight).

% remove_board_piece(Board, PieceToRemove, ResultBoard). Succeed if Piece is found in Board returning the ResultBoard without the Piece
remove_board_piece([Piece|Board], Piece, Board) :- !.
remove_board_piece([OtherPiece|Board], Piece, [OtherPiece|ResultBoard]) :- remove_board_piece(Board, Piece, ResultBoard).

% is_place_taken(Board, PosX, PosY, Height) Succeed if there is a piece at PosX PosY
is_place_taken([Piece|_], PosX, PosY, Height) :- 
    piece(PosX,PosY,_,_) = Piece,
    get_piece_Height(Piece, Height),
    !.
is_place_taken([_|Board], PosX, PosY, Height) :- is_place_taken(Board, PosX, PosY, Height).



% connected_board(Board) Succeed if Board is connected
connected_board([]).
connected_board([Piece|Board]) :- 
    piece(PosX, PosY,_,_) = Piece,
    get_all_pieces_list([Piece|Board], piece(PosX, PosY,_,_), Visited),
    connected_board([Piece|Board], [Piece], Visited).

connected_board(Board, [], Visited) :- length(Board, LengthB),
                                       length(Visited, LengthB),
                                       !.
connected_board(Board, [Piece|ToProcess], Visited) :- 
    pieces_together(Piece, Board, Neighbors),
    list_difference(Neighbors, Visited, NotVisitedNeigbors),
    concat_set_list(NotVisitedNeigbors, ToProcess, NewToProcess),
    first_element_list_or_empty_list(NewToProcess, NewVisitedItem),
    concat_set_list(NewVisitedItem, Visited, NewVisited),
    connected_board(Board, NewToProcess, NewVisited).


position_up_left(PosX, PosY, PosXNew, PosYNew) :- 0 is PosX mod 2,
                                                  PosXNew is PosX-1,
                                                  PosYNew is PosY-1. % Up Left
position_up_left(PosX, PosY, PosXNew, PosYNew) :- 1 is PosX mod 2,
                                                   PosXNew is PosX-1, % Up Left
                                                   PosYNew is PosY.

position_up(PosX, PosY, PosXNew, PosYNew) :- PosYNew is PosY-1, % Up
                                            PosXNew is PosX.

position_up_right(PosX, PosY, PosXNew, PosYNew) :- 0 is PosX mod 2,
                                                    PosXNew is PosX+1,
                                                    PosYNew is PosY-1. % Up Right
position_up_right(PosX, PosY, PosXNew, PosYNew) :- 1 is PosX mod 2,
                                                    PosXNew is PosX+1,
                                                    PosYNew is PosY. % Up Right

position_down_right(PosX, PosY, PosXNew, PosYNew) :- 1 is PosX mod 2,
                                                    PosXNew is PosX+1, 
                                                    PosYNew is PosY+1. % Down Right
position_down_right(PosX, PosY, PosXNew, PosYNew) :- 0 is PosX mod 2,
                                                   PosXNew is PosX+1, 
                                                   PosYNew is PosY. % Down Right

position_down(PosX, PosY, PosXNew, PosYNew) :- PosYNew is PosY+1, % Down
                                                PosXNew is PosX.

position_down_left(PosX, PosY, PosXNew, PosYNew) :- 1 is PosX mod 2,
                                                    PosXNew is PosX-1, 
                                                    PosYNew is PosY+1. % Down Left
position_down_left(PosX, PosY, PosXNew, PosYNew) :- 0 is PosX mod 2,
                                                    PosXNew is PosX-1, 
                                                    PosYNew is PosY. % Down Left
                                                    
% positions_next_to(Piece, PosX, PosY, Dir)
positions_next_to(PosX, PosY, PosXNew, PosYNew, up_l) :- position_up_left(PosX, PosY, PosXNew, PosYNew).
positions_next_to(PosX, PosY, PosXNew, PosYNew, up) :- position_up(PosX, PosY, PosXNew, PosYNew).
positions_next_to(PosX, PosY, PosXNew, PosYNew, up_r) :- position_up_right(PosX, PosY, PosXNew, PosYNew).
positions_next_to(PosX, PosY, PosXNew, PosYNew, dw_r) :- position_down_right(PosX, PosY, PosXNew, PosYNew).
positions_next_to(PosX, PosY, PosXNew, PosYNew, dw) :- position_down(PosX, PosY, PosXNew, PosYNew).
positions_next_to(PosX, PosY, PosXNew, PosYNew, dw_l) :- position_down_left(PosX, PosY, PosXNew, PosYNew).



% pieces_together(Piece1, Piece2) 
pieces_together(piece(PosX1, PosY1, _, _), piece(PosX2, PosY2, _, _)) :- positions_next_to(PosX1, PosY1, PosX2, PosY2, _).
pieces_together(piece(PosX1, PosY1, _, _), piece(PosX1, PosY1, _, _)).

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

% get_all_pieces_list(Pieces, Pattern, ListResult): Succeed if ListResult is the list of all pieces that unifies with with Pattern in Pieces
get_all_pieces_list([], _, []).
get_all_pieces_list([Piece|Rest], Pattern, Result) :- 
    not(Piece = Pattern),
    get_all_pieces_list(Rest, Pattern, Result),
    !.
get_all_pieces_list([Piece|Rest], Pattern, [Piece|Result]) :- 
    Piece = Pattern, 
    get_all_pieces_list(Rest, Pattern, Result).

% get_all_pieces_at(Board, PosX, PosY, List): Return all pieces in board that are in the given position
get_all_pieces_at(Board, PosX, PosY, List) :- get_all_pieces_list(Board, piece(PosX, PosY, _, _), List).

can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece) :- 
    piece(_, _, Color, Extra) = Piece,
    get_piece_Height(Piece, Height),
    get_position_max_Height_or_0(Board, NewPosX, NewPosY, MaxHeight),
    (
        MaxHeight >= Height,
        not((is_place_taken(Board, PosX1, PosY1, MaxHeight), 
             is_place_taken(Board, PosX2, PosY2, MaxHeight)))
        ;
        MaxHeight < Height,
        not((is_place_taken(Board, PosX1, PosY1, Height), 
             is_place_taken(Board, PosX2, PosY2, Height)))
    ),
    (
        is_place_taken(Board, NewPosX, NewPosY, MaxHeight),
        set_piece_Height(piece(NewPosX, NewPosY, Color, Extra), MaxHeight+1, SlidedPiece)
        ;
        not(is_place_taken(Board, NewPosX, NewPosY, MaxHeight)),
        set_piece_Height(piece(NewPosX, NewPosY, Color, Extra), MaxHeight, SlidedPiece)
    ).

% can_slide_into(Board, PosX, PosY, NewPosX, NewPosY, SlidedPiece) 
% Succeed if Piece can slide into NewPosX and NewPosY, returning the SlidedPiece.
% the slide makes the jumps according to the movement rules
can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to UpLeft
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 1,
    DirY = 1,
    0 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY-1,
    PosX2 is PosX-1, PosY2 is PosY,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).

can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to UpLeft
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 1,
    DirY = 0,
    1 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY-1,
    PosX2 is PosX-1, PosY2 is PosY+1,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).

can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Up
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 0,
    DirY = 1,
    0 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY-1,
    PosX2 is PosX+1, PosY2 is PosY-1,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).

can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Up
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 0,
    DirY = 1,
    1 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY,
    PosX2 is PosX+1, PosY2 is PosY,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).

can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Up Right
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = -1,
    DirY = 1,
    0 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY-1,
    PosX2 is PosX+1, PosY2 is PosY,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).

can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Up Right
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = -1,
    DirY = 0,
    1 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY-1,
    PosX2 is PosX+1, PosY2 is PosY+1,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).

can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Down Right
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = -1,
    DirY = 0,
    0 is PosX mod 2,
    PosX1 is PosX, PosY1 is PosY+1,
    PosX2 is PosX+1, PosY2 is PosY-1,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).
can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Down Right
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = -1,
    DirY = -1,
    1 is PosX mod 2,
    PosX1 is PosX+1, PosY1 is PosY,
    PosX2 is PosX, PosY2 is PosY+1,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).

can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Down
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 0,
    DirY = -1,
    0 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY,
    PosX2 is PosX+1, PosY2 is PosY,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).
can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Down
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 0,
    DirY = -1,
    1 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY+1,
    PosX2 is PosX+1, PosY2 is PosY+1,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).

can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Down Left
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 1,
    DirY = 0,
    0 is PosX mod 2,
    PosX1 is PosX+1, PosY1 is PosY+1,
    PosX2 is PosX, PosY2 is PosY+1,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).

can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to Down Left
    piece(PosX, PosY, _, _) = Piece,
    DirX is PosX - NewPosX,
    DirY is PosY - NewPosY,
    DirX = 1,
    DirY = -1,
    1 is PosX mod 2,
    PosX1 is PosX-1, PosY1 is PosY,
    PosX2 is PosX, PosY2 is PosY+1,
    can_slide_into_height(Board, Piece, PosX1, PosY1, PosX2, PosY2, NewPosX, NewPosY, SlidedPiece).
