:- module(piece_utils, [get_piece_X/2, get_piece_Y/2, get_piece_Color/2, 
    get_piece_Type/2, get_piece_Height/2]).

% Pieces
ant.
queen.
cricket.
spider.
beatle.

% Color
white.
black.

% Piece Definition
% piece(PosX, PosY, Color, [Type, Height]).
get_piece_X(piece(PosX,_,_,_), PosX).
get_piece_Y(piece(_,PosY,_,_), PosY).
get_piece_Color(piece(_,_,Color,_), Color).
get_piece_Type(piece(_,_,_,[Type|_]), Type).
get_piece_Height(piece(_,_,_,[_,Height|_]), Height).
