:- module(move_utils, [first_empty_place_from/7]).
:- use_module(board_utils). 


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
