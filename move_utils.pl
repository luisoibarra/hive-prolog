:- module(move_utils, [first_empty_place_from/7, border_move/3, move_above_and_finish_down/7]).
:- use_module(board_utils). 
:- use_module(piece_utils).
:- use_module(list_utils).

% first_empty_place_from(Board, PosX, PosY, Dir, Length, NewPosX, NewPosY) 
% Succeed if NewPosX and NewPosY are the coordinates of the first empty
% place moving in stright line in Dir direction from PosX and PosY in Board with Length movements
first_empty_place_from(Board, PosX, PosY, Dir, Length, NewPosX, NewPosY) :- 
    first_empty_place_from(Board, PosX, PosY, Dir, 0, Length, NewPosX, NewPosY). 

% Auxiliar
first_empty_place_from(Board, PosX, PosY, Dir, CurrLength, Length, NewPosX, NewPosY) :- 
    positions_next_to(PosX, PosY, TempPosX, TempPosY, Dir),
    is_place_taken(Board, TempPosX, TempPosY,_),
    first_empty_place_from(Board, TempPosX, TempPosY, Dir, CurrLength+1, Length, NewPosX, NewPosY).

first_empty_place_from(Board, PosX, PosY, Dir, CurrLength, Length, NewPosX, NewPosY) :- 
    positions_next_to(PosX, PosY, TempPosX, TempPosY, Dir),
    not(is_place_taken(Board, TempPosX, TempPosY,_)),
    Length is CurrLength+1,
    NewPosX = TempPosX,
    NewPosY = TempPosY.

% concat_positions_different(Pieces1, Pieces2, Result) Returns Pieces1-Pieces2=Diff, Diff+Pieces2=Result based on position 
concat_positions_different(Pieces1, Pieces2, Result) :- 
    positions_different(Pieces1, Pieces2, Diff),
    concat_list(Diff, Pieces2, Result).

% positions_different(Pieces1, Pieces2, Result) Returns Pieces1-Pieces2=Result based on position  
positions_different(Pieces1, Pieces2, Result) :- 
    findall(Piece1, 
        (
            member(Piece1, Pieces1),
            piece(InX, InY, _, _) = Piece1,
            get_all_pieces_at(Pieces2,InX,InY,SamePositionPieces),
            length(SamePositionPieces, 0)
        ), Result).
    

% border_move(Board, Piece, Result) Returns the pieces and the distances from Piece that moves sliding around the Board border
border_move(Board, Piece, Result) :- 
    border_move(Board, [[Piece,0]], [Piece], PreResult),
    [_|Result] = PreResult.

border_move(_, [], _, []).
border_move(Board, [[Piece, Distance]|ToProcess], Visited, [[Piece, Distance]|Result]) :- 
    piece(PosX, PosY,_,_) = Piece,
    get_piece_Height(Piece,Height),

    findall(NewPiece, 
            (positions_next_to(PosX, PosY, X, Y,_), 
            not(is_place_taken(Board, X, Y, Height)),
            can_slide_into(Board, Piece, X,Y,NewPiece),
            pieces_together(NewPiece, Board, Neighbors),
            length(Neighbors, Length),
            Length > 0
            ),
            NewPieces),
    
    positions_different(NewPieces, Visited, NotVisited),
    NewDistance is Distance + 1,
    zip_const(NotVisited, NewDistance, AddToProcess),
    concat_set_list(ToProcess, AddToProcess, NewToProcess),
    (
        concat_positions_different(NotVisited,Visited,NewVisited)
        ;
        true
    ),
    border_move(Board, NewToProcess, NewVisited,Result).

move_above_and_finish_down(PosX, PosY, BaseHeight, MaxStepsAbove, Board, NewPosX, NewPosY) :-
    move_above_and_finish_down(BaseHeight, MaxStepsAbove, Board, [[PosX, PosY, 0]], [], ReturnInfo),
    member([NewPosX, NewPosY], ReturnInfo).

% move_above_and_finish_down(BaseHeight, MaxStepsAbove, Board, ToProcess, Visited, ReturnInfo)
move_above_and_finish_down(_, _, _, [], _, []).
move_above_and_finish_down(BaseHeight, MaxStepsAbove, Board, [[CurrentX, CurrentY, StepNumber]|ToProcess], Visited, ReturnInfo) :-
    StepNumber < MaxStepsAbove,
    SlideHeight is BaseHeight + 1,
    findall(pos(X,Y), positions_next_to(CurrentX, CurrentY, X, Y, _), AroundPositions),
    findall(P, (
        member(pos(X,Y), AroundPositions), 
        get_top_piece_at(Board, X, Y, P), 
        get_piece_Height(P,BaseHeight),
        can_slide_into_height(Board, CurrentX, CurrentY, SlideHeight, _, _, X, Y, _),
        not(member(P, Visited))), TopPiecesAround),
    NextStepNumber is StepNumber + 1,
    findall([X,Y,NextStepNumber], (member(piece(X, Y, _, _), TopPiecesAround)), AddToProcess),
    concat_list(Visited, TopPiecesAround, NewVisited),
    concat_list(ToProcess, AddToProcess, NewToProcess),
    move_above_and_finish_down(BaseHeight, MaxStepsAbove, Board, NewToProcess, NewVisited, ReturnInfo).

move_above_and_finish_down(BaseHeight, MaxStepsAbove, Board, [[CurrentX, CurrentY, StepNumber]|ToProcess], Visited, ReturnInfo) :-
    StepNumber = MaxStepsAbove,
    SlideHeight is BaseHeight + 1,
    findall(pos(X,Y), positions_next_to(CurrentX, CurrentY, X, Y, _), AroundPositions),
    findall([X,Y], (
        member(pos(X,Y), AroundPositions),
        (
            get_top_piece_at(Board, X, Y, P),
            get_piece_Height(P,Height),
            Height < BaseHeight
            ;
            not(is_place_taken(Board, X, Y, 0))
        ),
        can_slide_into_height(Board, CurrentX, CurrentY, SlideHeight, _, _, X, Y, _)
        ), PositionsAvailable),
    move_above_and_finish_down(BaseHeight, MaxStepsAbove, Board, ToProcess, Visited, PrevReturnInfo),
    concat_set_list(PrevReturnInfo, PositionsAvailable, ReturnInfo).

% Auxiliar
