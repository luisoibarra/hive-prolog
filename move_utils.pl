:- module(move_utils, [first_empty_place_from/7]).
:- use_module(board_utils). 
:- use_module(piece_utils).

% first_empty_place_from(Board, PosX, PosY, Dir, Length, NewPosX, NewPosY) 
% Succeed if NewPosX and NewPosY are the coordinates of the first empty
% place moving in stright line in Dir direction from PosX and PosY in Board with Length movements
first_empty_place_from(Board, PosX, PosY, Dir, Length, NewPosX, NewPosY) :- 
    first_empty_place_from(Board, PosX, PosY, Dir, 0, Length, NewPosX, NewPosY). 

% Auxiliar
first_empty_place_from(Board, PosX, PosY, Dir, CurrLength, Length, NewPosX, NewPosY) :- 
    positions_next_to(PosX, PosY, TempPosX, TempPosY, Dir),
    is_place_taken(Board, TempPosX, TempPosY,_),
    first_empty_place_from(Board, TempPosX, TempPosY, Dir, CurrLength+1, Length, NewPosX, NewPosY),
    !.

first_empty_place_from(Board, PosX, PosY, Dir, CurrLength, Length, NewPosX, NewPosY) :- 
    positions_next_to(PosX, PosY, TempPosX, TempPosY, Dir),
    not(is_place_taken(Board, TempPosX, TempPosY,_)),
    Length is CurrLength+1,
    NewPosX = TempPosX,
    NewPosY = TempPosY.


% border_move(Board, PosX, PosY, MinLength, BorderX, BorderY) Returns the borders positions sliding from PosX,PosY and the MinLength to arrive there.

border_move(Board, [], Visited, Result).
border_move(Board, [Piece|ToProcess], Visited, Result) :- 
    piece(PosX, PosY,_,_) = Piece,
    get_piece_Height(Piece,Height),

    pieces_together(Piece, Board, Neighbors),

    positions_next_to(PosX, PosY, NewPosX, NewPosY, _),
    not(is_place_taken(Board, NewPosX, NewPosY, Height)),
    can_slide_into(Board, Piece, NewPosX, NewPosY, SlidedPiece). % TODO POR AQUI

    % pieces_together(Piece, Board, Neighbors),
    % list_difference(Neighbors, Visited, NotVisitedNeigbors),
    % concat_set_list(NotVisitedNeigbors, ToProcess, NewToProcess),
    % first_element_list_or_empty_list(NewToProcess, NewVisitedItem),
    % concat_set_list(NewVisitedItem, Visited, NewVisited),
    % connected_board(Board, NewToProcess, NewVisited).
% Auxiliar
