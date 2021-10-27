:- module(run_game_utils,[make_a_play/5, get_game_GameHistory/2, get_game_Turn/2]).
:- use_module(add_piece_rules). 
:- use_module(game_rules). 
:- use_module(move_piece_rules). 
:- use_module(board_utils). 
:- use_module(list_utils). 
:- use_module(piece_utils). 


% game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces, GameHistory, Turn]).
get_game_GameHistory(Game, GameHistory) :- 
    game(_, _, [_,_, GameHistory|_]) = Game.
get_game_Turn(Game, Turn) :- 
    game(_, _, [_,_,_,Turn|_]) = Game.


switch_player(white, black). 
switch_player(black, white). 

% get_piece_type_to_play(CurrentPlayer, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, NewWhiteTypePieces, NewBlackTypePieces) 
get_piece_type_to_play(white, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, NewWhiteTypePieces, BlackTypePieces) :- 
    element_at(WhiteTypePieces, PiecePosition, PieceType),
    remove_at(WhiteTypePieces, PiecePosition, NewWhiteTypePieces).
get_piece_type_to_play(black, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, WhiteTypePieces, NewBlackTypePieces) :- 
    element_at(BlackTypePieces, PiecePosition, PieceType),
    remove_at(BlackTypePieces, PiecePosition, NewBlackTypePieces).

% set_piece(PiecePosition, PosX, PosY, Game, NewGame)
set_piece(PiecePosition, PosX, PosY, Game, NewGame) :- 
    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces, GameHistory, Turn|Extra]) = Game,
    
    get_piece_type_to_play(CurrentPlayer, PiecePosition, WhiteTypePieces, BlackTypePieces, PieceType, NewWhiteTypePieces, NewBlackTypePieces),
    build_piece(PosX, PosY, CurrentPlayer, [PieceType, 0], Piece),
    add_piece(Board, Piece, NewBoard),

    switch_player(CurrentPlayer, NewCurrentPlayer),
    NewTurn is Turn + 1,
    game(NewBoard, NewCurrentPlayer, [NewWhiteTypePieces, NewBlackTypePieces, [Game|GameHistory], NewTurn|Extra]) = NewGame.

% move_piece(PosX, PosY, DestPosX, DestPosY, Game, NewGame) 
move_piece(PosX, PosY, DestPosX, DestPosY, Game, NewGame) :- 
    game(Board, CurrentPlayer, [WhiteTypePieces, BlackTypePieces, GameHistory, Turn|Extra]) = Game,
    get_top_piece_at(Board, PosX, PosY, PieceToMove),
    get_piece_Color(PieceToMove, CurrentPlayer),

    move(Board, PieceToMove, DestPosX, DestPosY, _, NewBoard),

    switch_player(CurrentPlayer, NewCurrentPlayer),
    NewTurn is Turn + 1,
    game(NewBoard, NewCurrentPlayer, [WhiteTypePieces, BlackTypePieces, [Game|GameHistory], NewTurn|Extra]) = NewGame.

end_turn_feedback(Game, Feedback, GameStatus) :-

    game(Board, CurrentPlayer, [_, _, GameHistory|_]) = Game,
    switch_player(PrevPlayer, CurrentPlayer),
    (
        queen_surrounded(Board, PrevPlayer),
        string_concat('Game Over ', PrevPlayer, Feedback),
        GameStatus = over
        ;
        queen_surrounded(Board, CurrentPlayer),
        string_concat('Game Over ', CurrentPlayer, Feedback),
        GameStatus = over
        ;
        repeated_game_positions(GameHistory, Amount),
        Amount >= 3,
        Feedback = 'Game Over, repeated positions detected. Tie',
        GameStatus = tie
        ; % TODO Missing no possible moves
        Feedback = '',
        GameStatus = continue
    ).


% make_a_play(Play, Game, NewGame, Feedback, GameStatus)
make_a_play(set_play(PositionSelectedPieceToSet, PosX, PosY), Game, NewGame, Feedback, GameStatus) :-
    (
        set_piece(PositionSelectedPieceToSet, PosX, PosY, Game, NewGame),
        end_turn_feedback(NewGame, Feedback, GameStatus)
        ;
        % If HERE set_piece is false
        NewGame = Game,
        Feedback = 'Invalid piece set',
        GameStatus = invalid
    ).

make_a_play(move_play(PosX, PosY, NewPosX, NewPosY), Game, NewGame, Feedback, GameStatus) :-
    (
        move_piece(PosX, PosY, NewPosX, NewPosY, Game, NewGame),
        end_turn_feedback(NewGame, Feedback, GameStatus)
        ;
        % If HERE move_piece is false
        NewGame = Game,
        Feedback = 'Invalid piece move',
        GameStatus = invalid
    ).

