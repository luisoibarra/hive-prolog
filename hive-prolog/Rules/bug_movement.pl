:- module(bug_movement, [
        bug_movement_functor/2,
        queen_moves_position/7,
        grasshopper_moves_position/7,
        beetle_moves_position/7,
        spider_moves_position/7,
        ant_moves_position/7,
        mosquito_moves_position/7,
        ladybug_moves_position/7,
        pillbug_moves_position/7
    ]).
:- use_module('../Utils/board_utils'). 
:- use_module('../Utils/list_utils'). 
:- use_module('../Utils/piece_utils'). 
:- use_module(move_utils). 

% Bugs movement
bug_movement_functor(queen_moves_position, queen).
bug_movement_functor(grasshopper_moves_position, grasshopper).
bug_movement_functor(beetle_moves_position, beetle).
bug_movement_functor(spider_moves_position, spider).
bug_movement_functor(ant_moves_position, ant).
bug_movement_functor(mosquito_moves_position, mosquito).
bug_movement_functor(ladybug_moves_position, ladybug).
bug_movement_functor(pillbug_moves_position, pillbug).

% Queen Movement
queen_moves_position(Board, Piece, [], NewPosX, NewPosY, NewPiece, NewBoard) :- 
    slide_one_step(Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard).

% Grasshopper Movement
grasshopper_moves_position(Board, Piece, [], NewPosX, NewPosY, NewPiece, NewBoard) :-
  
    piece(PosX, PosY,Color,Extra) = Piece,
    first_empty_place_from(Board, PosX, PosY, _, Length, NewPosX, NewPosY),
    piece(NewPosX, NewPosY, Color, Extra) = NewPiece,
    Length > 1,
    remove_board_piece(Board, Piece, RemovedBoard),
    [NewPiece|RemovedBoard] = NewBoard.

% Beetle Movement
beetle_moves_position(Board, Piece, [], NewPosX, NewPosY, NewPiece, NewBoard) :- 
    piece(PosX, PosY,_,_) = Piece,
    positions_next_to(PosX, PosY, NewPosX, NewPosY,_),
    can_slide_into(Board, Piece, NewPosX, NewPosY, NewPiece),
    remove_board_piece(Board, Piece, RemovedBoard),
    NewBoard = [NewPiece|RemovedBoard].

% Spider Movement
spider_moves_position(Board, Piece, [], NewPosX, NewPosY, NewPiece, NewBoard) :- 
    border_move(Board, Piece, BorderMoves),
    get_all_pieces_list(BorderMoves, [_,3], SpiderMovesWithDistance),
    unzip(SpiderMovesWithDistance, SpiderMoves, _),
    !,
    member(NewPiece, SpiderMoves),
    piece(NewPosX, NewPosY, _,_) = NewPiece,
    remove_board_piece(Board, Piece, RemovedBoard),
    NewBoard = [NewPiece|RemovedBoard].

% Ant Movement
ant_moves_position(Board, Piece, [], NewPosX, NewPosY, NewPiece, NewBoard) :- 
    border_move(Board, Piece, BorderMoves),
    unzip(BorderMoves, AntMoves, _),
    !,
    member(NewPiece, AntMoves),
    piece(NewPosX, NewPosY, _,_) = NewPiece,
    remove_board_piece(Board, Piece, RemovedBoard),
    NewBoard = [NewPiece|RemovedBoard].

% Mosquito Movement
mosquito_moves_position(Board, Piece, ExtraArgs, NewPosX, NewPosY, NewPiece, NewBoard) :-
    piece(PosX, PosY, _, _) = Piece,
    findall(pos(X,Y), positions_next_to(PosX, PosY, X, Y, _), AroundPositions),
    findall(P, (member(pos(X,Y), AroundPositions), get_top_piece_at(Board, X, Y, P)), TopPiecesAround),
    findall(X, (member(Y, TopPiecesAround), get_piece_Type(Y, Type), bug_movement_functor(X, Type)), AroundMovementFunctors),

    list_to_set(AroundMovementFunctors, MosquitoAroundMovementFunctors),
    bug_movement_functor(MosquitoFunctor, mosquito),
    list_difference(MosquitoAroundMovementFunctors, [MosquitoFunctor], InitialFunctorList),

    get_piece_Height(Piece, Height),
    (
        Height > 0,
        bug_movement_functor(BeetleFunctor, beetle),
        concat_set_list(InitialFunctorList, [BeetleFunctor], FunctorListWithBeetle),
        FunctorList = FunctorListWithBeetle
        ;
        Height = 0,
        FunctorList = InitialFunctorList
    ),
    member(Functor, FunctorList),
    Function =.. [Functor, Board, Piece, ExtraArgs, NewPosX, NewPosY, NewPiece, NewBoard],
    call(Function).

% Ladybug Movement
ladybug_moves_position(Board, Piece, [], NewPosX, NewPosY, NewPiece, NewBoard) :-
    piece(_, _, Color, [Type, _|Extra]) = Piece,
    remove_board_piece(Board, Piece, BoardWithNoLadybug),
    move_above_and_finish_down(Piece, 2, BoardWithNoLadybug, NewPosX, NewPosY),
    get_position_max_Height_or_default(Board, NewPosX, NewPosY, -1, MaxHeight),
    NewHeight is MaxHeight + 1,
    build_piece(NewPosX, NewPosY, Color, [Type, NewHeight|Extra], NewPiece),
    NewBoard = [NewPiece|BoardWithNoLadybug].

% Pillbug Movement
pillbug_moves_position(Board, Piece, ExtraArgs, NewPosX, NewPosY, NewPiece, NewBoard) :-
    (
        ExtraArgs = [],
        slide_one_step(Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard)
        ;
        ExtraArgs = [PieceToMoveX, PieceToMoveY],
        pillbug_translate(Piece, Board, PieceToMoveX, PieceToMoveY, MovedPiece, NewBoard),
        piece(NewPosX, NewPosY, _, _) = MovedPiece,
        NewPiece = Piece
        % piece(NewPosX, NewPosY, _, _) = NewPiece
    ).
