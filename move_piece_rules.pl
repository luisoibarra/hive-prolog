:- module(move_piece_rules, [move/6]).
:- use_module(board_utils). 
:- use_module(list_utils). 
:- use_module(piece_utils). 
:- use_module(move_utils). 

% Movement Rules

% top_at_position(Board, PieceToMove). Succeed if PieceToMove is at the top position height 
top_at_position(OldBoard, Piece) :-
    piece(PosX, PosY, _, _) = Piece,
    get_piece_Height(Piece, Height), 
    not((get_all_pieces(OldBoard, piece(PosX, PosY, _, _), piece(PosX, PosY, _, [_, Height2|_])), Height < Height2)).

% queen_present(Board, Piece) Succeed if there is a queen of the piece color in Board. 
queen_present(Board, Piece) :- 
    get_piece_Color(Piece, Color),
    member(piece(_,_,Color,[queen|_]), Board), 
    !.
    

% connected_board_if_removed(Board, Piece) Succeed if Board is connected after removing Piece
connected_board_if_removed(Board, Piece) :- 
    remove_board_piece(Board, Piece, ResultBoard),
    connected_board(ResultBoard).

% single_hive_after(Board, PieceToRemove, PieceToAdd) Succeed if Board fulfil Unique Hive Rule 
single_hive_after(Board, PieceToRemove, PieceToAdd) :- 
    remove_board_piece(Board, PieceToRemove, ResultBoard),
    connected_board(ResultBoard),
    connected_board([PieceToAdd|ResultBoard]).

% pre_move_rules(Board, Piece) Rules that must fulfil all pieces on given Board to be able to move
pre_move_rules(Board, Piece) :- 
    top_at_position(Board, Piece),
    queen_present(Board, Piece).

% post_move_rules(OldBoard, OldPiece, NewPiece, NewBoard) Rules that must be fulfilled after the piece is moved
post_move_rules(_, _, _, NewBoard) :- 
    connected_board(NewBoard). % Single Hive Simplified
    % single_hive_after(OldBoard, OldPiece, NewPiece).


% Bugs movement
bug_movement_functor(queen_moves_position, queen).
bug_movement_functor(cricket_moves_position, cricket).
bug_movement_functor(beetle_moves_position, beetle).
bug_movement_functor(spider_moves_position, spider).
bug_movement_functor(ant_moves_position, ant).
bug_movement_functor(mosquito_moves_position, mosquito).
bug_movement_functor(ladybug_moves_position, ladybug).

% Queen Movement
queen_moves_position(Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard) :- 
    pre_move_rules(Board, Piece),

    piece(PosX, PosY,_,_) = Piece,
    get_piece_Height(Piece, PieceHeight),
    positions_next_to(PosX, PosY, NewPosX, NewPosY,_),
    not(is_place_taken(Board, NewPosX, NewPosY,_)),
    can_slide_into(Board, Piece, NewPosX, NewPosY, NewPiece),
    get_piece_Height(NewPiece, PieceHeight),
    remove_board_piece(Board, Piece, RemovedBoard),
    [NewPiece|RemovedBoard] = NewBoard,

    post_move_rules(Board, Piece, NewPiece, NewBoard).

% Cricket Movement
cricket_moves_position(Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard) :-
    pre_move_rules(Board, Piece),
  
    piece(PosX, PosY,Color,Extra) = Piece,
    first_empty_place_from(Board, PosX, PosY, _, Length, NewPosX, NewPosY),
    piece(NewPosX, NewPosY, Color, Extra) = NewPiece,
    Length > 1,
    remove_board_piece(Board, Piece, RemovedBoard),
    [NewPiece|RemovedBoard] = NewBoard,

    post_move_rules(Board, Piece, NewPiece, NewBoard).

% Beetle Movement
beetle_moves_position(Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard) :- 
    pre_move_rules(Board, Piece),

    piece(PosX, PosY,_,_) = Piece,
    positions_next_to(PosX, PosY, NewPosX, NewPosY,_),
    can_slide_into(Board, Piece, NewPosX, NewPosY, NewPiece),
    remove_board_piece(Board, Piece, RemovedBoard),
    NewBoard = [NewPiece|RemovedBoard],

    post_move_rules(Board, Piece, NewPiece, NewBoard).

% Spider Movement
spider_moves_position(Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard) :- 
    pre_move_rules(Board, Piece),

    border_move(Board, Piece, BorderMoves),
    get_all_pieces_list(BorderMoves, [_,3], SpiderMovesWithDistance),
    unzip(SpiderMovesWithDistance, SpiderMoves, _),
    !,
    member(NewPiece, SpiderMoves),
    piece(NewPosX, NewPosY, _,_) = NewPiece,
    remove_board_piece(Board, Piece, RemovedBoard),
    NewBoard = [NewPiece|RemovedBoard],

    post_move_rules(Board, Piece, NewPiece, NewBoard).

% Ant Movement
ant_moves_position(Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard) :- 
    pre_move_rules(Board, Piece),

    border_move(Board, Piece, BorderMoves),
    unzip(BorderMoves, AntMoves, _),
    !,
    member(NewPiece, AntMoves),
    piece(NewPosX, NewPosY, _,_) = NewPiece,
    remove_board_piece(Board, Piece, RemovedBoard),
    NewBoard = [NewPiece|RemovedBoard],

    post_move_rules(Board, Piece, NewPiece, NewBoard).

% Mosquito Movement
mosquito_moves_position(Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard) :-
    pre_move_rules(Board, Piece),

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
    Function =.. [Functor, Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard],
    call(Function),

    post_move_rules(Board, Piece, NewPiece, NewBoard).

% Ladybug Movement
ladybug_moves_position(Board, Piece, NewPosX, NewPosY, NewPiece, NewBoard) :-
    pre_move_rules(Board, Piece),

    piece(PosX, PosY, Color, [Type, Height|Extra]) = Piece,
    remove_board_piece(Board, Piece, BoardWithNoLadybug)
    move_above_and_finish_down(PosX, PosY, Height, 2, BoardWithNoLadybug, NewPosX, NewPosY),
    get_position_max_Height_or_default(Board, NewPosX, NewPosY, -1, MaxHeight),
    NewHeight is MaxHeight + 1,
    build_piece(NewPosX, NewPosY, Color, [Type, NewHeight|Extra], NewPiece),
    NewBoard = [NewPiece|BoardWithNoLadybug],

    post_move_rules(Board, Piece, NewPiece, NewBoard).


% move(Board, OldPiece, NewPiece, NewBoard) Return the NewBoard after the move is made.
move(Board, OldPiece, NewPosX, NewPosY, NewPiece, NewBoard) :- 
    get_piece_Type(OldPiece, Type),
    bug_movement_functor(Functor, Type),
    Function =.. [Functor, Board, OldPiece, NewPosX, NewPosY, NewPiece, NewBoard],
    call(Function).

