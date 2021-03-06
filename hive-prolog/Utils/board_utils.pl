:- module(board_utils, 
    [remove_board_piece/3, is_place_taken/4, connected_board/1, positions_next_to/5,
    pieces_together/3, can_slide_into/5, can_slide_into_height/9, get_first_piece/3, exist_queen/1, color_played_list/3,
    placed_around_of/3, get_all_pieces_list/3, get_all_pieces_at/4, get_top_piece_at/4, get_position_max_Height_or_default/5,
    queen_surrounding_pieces/3]).
:- use_module(list_utils). 
:- use_module(piece_utils). 


% get_position_max_Height_or_default(Board, PosX, PosY, Default, MaxHeight): Returns the MaxHeight of all pieces at PosX, PosY in Board
get_position_max_Height_or_default(Board, PosX, PosY, Default, MaxHeight) :- 
    get_all_pieces_at(Board, PosX, PosY, PosPieces),
    get_position_max_Height_or_default(PosPieces, Default, MaxHeight).

% get_position_max_Height_or_default(PositionPieces, CurrentMax, MaxHeight): Returns in the maximun height of all PositionPieces 
get_position_max_Height_or_default([], Current, Current).
get_position_max_Height_or_default([Piece|Rest], CurrentMax, MaxHeight) :- 
    get_piece_Height(Piece, Height),
    Height > CurrentMax,
    get_position_max_Height_or_default(Rest, Height, MaxHeight).
get_position_max_Height_or_default([Piece|Rest], CurrentMax, MaxHeight) :- 
    get_piece_Height(Piece, Height),
    not(Height > CurrentMax),
    get_position_max_Height_or_default(Rest, CurrentMax, MaxHeight).

% remove_board_piece(Board, PieceToRemove, ResultBoard). Succeed if Piece is found in Board returning the ResultBoard without the Piece
remove_board_piece([Piece|Board], Piece, Board) :- !.
remove_board_piece([OtherPiece|Board], Piece, [OtherPiece|ResultBoard]) :- remove_board_piece(Board, Piece, ResultBoard).

% is_place_taken(Board, PosX, PosY, Height) Succeed if there is a piece at PosX PosY
is_place_taken([Piece|_], PosX, PosY, Height) :- 
    piece(PosX,PosY,_,_) = Piece,
    get_piece_Height(Piece, Height),
    !.
is_place_taken([_|Board], PosX, PosY, Height) :- is_place_taken(Board, PosX, PosY, Height).

% get_top_piece_at(Board, PosX, PosY, Piece) Return the heighest piece at given position  
get_top_piece_at(Board, PosX, PosY, Piece) :- 
    get_position_max_Height_or_default(Board, PosX, PosY, 0, Height),
    get_first_piece(Board, piece(PosX, PosY, _,[_,Height|_]), Piece).

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

% dir_offset(Dir, PosX, OffsetX, OffsetY): Offset for every direction according PosX
dir_offset(up_l, PosX, -1, -1) :- 0 is PosX mod 2.
dir_offset(up_l, PosX, -1, 0) :- 1 is PosX mod 2.
dir_offset(up, _, 0, -1).
dir_offset(up_r, PosX, 1, -1) :- 0 is PosX mod 2.
dir_offset(up_r, PosX, 1, 0) :- 1 is PosX mod 2.
dir_offset(dw_r, PosX, 1, 0) :- 0 is PosX mod 2.
dir_offset(dw_r, PosX, 1, 1) :- 1 is PosX mod 2.
dir_offset(dw, _, 0, 1).
dir_offset(dw_l, PosX, -1, 0) :- 0 is PosX mod 2.
dir_offset(dw_l, PosX, -1, 1) :- 1 is PosX mod 2.

% complementary_directions(MainDir, ComplDir1, ComplDir2) Returns the directions surrounding the MainDir
complementary_directions(up_l, dw_l, up).
complementary_directions(up, up_l, up_r).
complementary_directions(up_r, up, dw_r).
complementary_directions(dw_r, up_r, dw).
complementary_directions(dw, dw_r, dw_l).
complementary_directions(dw_l, dw, up_l).

% positions_next_to(PosX, PosY, PosXNew, PosYNew, Dir)
positions_next_to(PosX, PosY, PosXNew, PosYNew, Dir) :- 
    dir_offset(Dir, PosX, OffsetX, OffsetY),
    PosXNew is PosX + OffsetX,
    PosYNew is PosY + OffsetY.



% pieces_together(Piece1, Piece2) 
pieces_together(piece(PosX1, PosY1, _, _), piece(PosX2, PosY2, _, _)) :- positions_next_to(PosX1, PosY1, PosX2, PosY2, _).
pieces_together(piece(PosX1, PosY1, _, _), piece(PosX1, PosY1, _, _)).

% pieces_together(Piece, Board, PiecesTogether)
pieces_together(piece(_, _, _, _), [], []) :- !.
pieces_together(Piece1, [Piece2|Pieces], [Piece2|Result]) :- pieces_together(Piece1, Piece2), 
                                                             pieces_together(Piece1, Pieces, Result), !.
pieces_together(Piece1, [_|Pieces], Result) :- pieces_together(Piece1, Pieces, Result).

% placed_around_of(Board, Piece, AroundPiece): Return the pieces that are next to Piece in Board
placed_around_of([BoardPiece|_], Piece, BoardPiece) :- 
    pieces_together(BoardPiece, Piece),
    piece(PosX1, PosY1, _, _) = BoardPiece,
    piece(PosX2, PosY2, _, _) = Piece,
    not((PosX1=PosX2,PosY1=PosY2)).

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

% queen_surrounding_pieces(Board, Color, Pieces) Returns all the Pieces in the surroundings of Color queen
queen_surrounding_pieces(Board, Color, Pieces) :-
    get_first_piece(Board, piece(_,_,Color,[queen|_]), Queen),
    !,
    get_piece_Height(Queen, Height),
    piece(PosX, PosY, _, _) = Queen,
    findall(X, 
        (member(X, Board),
         piece(PosX1, PosX2, _, [_,Height|_]) = X,
         positions_next_to(PosX, PosY, PosX1, PosX2,_)
        ), Pieces).
queen_surrounding_pieces(_, _, []). % Queen doesn't exist

% get_first_piece(Pieces, Pattern, Result): Succeed if a piece in Pieces unifies with Pattern
get_first_piece([First|_], PiecePatter, First) :- findall(X, PiecePatter = X, [First]).
get_first_piece([_|Pieces], PiecePatter, Result) :- get_first_piece(Pieces, PiecePatter, Result).

% get_all_pieces_list(Pieces, Pattern, ListResult): Succeed if ListResult is the list of all pieces that unifies with with Pattern in Pieces
get_all_pieces_list(List, Pattern, Result) :- 
    unification_filter(List, Pattern, Result).

% get_all_pieces_at(Board, PosX, PosY, List): Return all pieces in board that are in the given position
get_all_pieces_at(Board, PosX, PosY, List) :- 
    integer(PosX), integer(PosY), !,
    get_all_pieces_list(Board, piece(PosX, PosY, _, _), List).
get_all_pieces_at(Board, PosX, PosY, Elem) :- 
    aux_get_all_pieces_at(Board, Elements),
    member(Elem, Elements),
    [Item|_] = Elem,
    piece(PosX, PosY, _, _) = Item.

aux_get_all_pieces_at([],[]).
aux_get_all_pieces_at(Board, [List|Elements]) :- 
    Board = [Element|_],
    piece(PosX, PosY, _, _) = Element,
    get_all_pieces_at(Board, PosX, PosY, List),
    remove_all(Board, piece(PosX, PosY, _, _), NewBoard),
    aux_get_all_pieces_at(NewBoard, Elements).


% can_slide_into(Board, PosX, PosY, NewPosX, NewPosY, SlidedPiece) 
% Succeed if Piece can slide into NewPosX and NewPosY, returning the SlidedPiece.
% the slide makes the jumps according to the movement rules
can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece) :- % Slide to UpLeft
    piece(PosX, PosY, Color, Extra) = Piece,
    get_piece_Height(Piece, Height),
    can_slide_into_height(Board, PosX, PosY, Height, Color, Extra, NewPosX, NewPosY, SlidedPiece).

can_slide_into_height(Board, PosX, PosY, Height, Color, Extra, NewPosX, NewPosY, SlidedPiece) :- 
    DirX is NewPosX - PosX,
    DirY is NewPosY - PosY,
    dir_offset(MainDir, PosX, DirX, DirY),
    complementary_directions(MainDir, ComplDir1, ComplDir2),
    dir_offset(ComplDir1, PosX, OffComplX1, OffComplY1),
    dir_offset(ComplDir2, PosX, OffComplX2, OffComplY2),
    PosX1 is PosX + OffComplX1, PosY1 is PosY + OffComplY1,
    PosX2 is PosX + OffComplX2, PosY2 is PosY + OffComplY2,

    get_position_max_Height_or_default(Board, NewPosX, NewPosY, 0, MaxHeight),
    (
        MaxHeight >= Height,
        is_place_taken(Board, NewPosX, NewPosY, MaxHeight),
        SearchHeight is MaxHeight+1,
        not((is_place_taken(Board, PosX1, PosY1, SearchHeight), 
             is_place_taken(Board, PosX2, PosY2, SearchHeight))),
        set_piece_Height(piece(NewPosX, NewPosY, Color, Extra), SearchHeight, SlidedPiece)
        ;
        MaxHeight >= Height,
        not(is_place_taken(Board, NewPosX, NewPosY, MaxHeight)),
        not((is_place_taken(Board, PosX1, PosY1, MaxHeight), 
             is_place_taken(Board, PosX2, PosY2, MaxHeight))),
        set_piece_Height(piece(NewPosX, NewPosY, Color, Extra), MaxHeight, SlidedPiece)
        ;
        MaxHeight < Height,
        not((is_place_taken(Board, PosX1, PosY1, Height), 
             is_place_taken(Board, PosX2, PosY2, Height)))
    ),
    (
        MaxHeight >= Height
        ;
        MaxHeight < Height,
        is_place_taken(Board, NewPosX, NewPosY, MaxHeight),
        NewHeight is MaxHeight + 1,
        set_piece_Height(piece(NewPosX, NewPosY, Color, Extra), NewHeight, SlidedPiece)
        ;
        MaxHeight < Height,
        not(is_place_taken(Board, NewPosX, NewPosY, MaxHeight)),
        set_piece_Height(piece(NewPosX, NewPosY, Color, Extra), MaxHeight, SlidedPiece)
    ).
