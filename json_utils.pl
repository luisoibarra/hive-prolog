:- module(json_utils, [
    convert_action_to_json/2, convert_action_from_json/2,
    convert_game_to_json/2, convert_pieces_to_json/2,
    convert_piece_to_json/2, convert_players_remaining_pieces_to_json/2
    ]).

% CONVERT FROM JSON %

convert_action_from_json(ActionJson, Action) :-
    json(Properties) = ActionJson,
    member(type=set, Properties),
    member(final_x=PosX, Properties),
    member(final_y=PosY, Properties),
    member(piece_index=PieceIndex, Properties),
    Action = set_play(PieceIndex, PosX, PosY).
convert_action_from_json(ActionJson, Action) :-
    json(Properties) = ActionJson,
    member(type=move, Properties),
    member(from_x=PosX, Properties),
    member(from_y=PosY, Properties),
    member(final_x=NewPosX, Properties),
    member(final_y=NewPosY, Properties),
    Action = move_play(PosX, PosY, NewPosX, NewPosY).


% CONVERT FROM JSON %

% CONVERT TO JSON %
convert_game_to_json(Game, GameJson) :-
    game(Board, CurrentPlayer, [PlayerPieces, _, Turn|_]) = Game,
    convert_pieces_to_json(Board, BoardJson),
    convert_players_remaining_pieces_to_json(PlayerPieces, PlayerPiecesJson),
    GameJson = json([turn=Turn, board=BoardJson, player=CurrentPlayer, remaining_pieces=PlayerPiecesJson]).

convert_pieces_to_json([], []).
convert_pieces_to_json([Piece|Pieces], [PieceJson|PiecesJson]) :- 
    convert_piece_to_json(Piece, PieceJson),
    convert_pieces_to_json(Pieces, PiecesJson).

convert_piece_to_json(Piece, PieceJson) :- 
    piece(PosX, PosY, Color, [Type, Height]) = Piece,
    PieceJson = json([x=PosX, y=PosY, color=Color, type=Type, height=Height]).

convert_players_remaining_pieces_to_json([], []).
convert_players_remaining_pieces_to_json([PieceInfo|PlayerPieces], [PieceInfJson|PlayerPiecesJson]) :- 
    pieces_info(PlayerColor, RemainingPieces) = PieceInfo,
    PieceInfJson = json([player=PlayerColor, pieces=RemainingPieces]),
    convert_players_remaining_pieces_to_json(PlayerPieces, PlayerPiecesJson).

convert_action_to_json(Action, ActionJson) :-
    move_play(PosX, PosY, NewPosX, NewPosY) = Action,
    ActionJson = json([type=move, from_x=PosX, from_y=PosY, final_x=NewPosX, final_y=NewPosY]).
convert_action_to_json(Action, ActionJson) :-
    set_play(PiecePos, PosX, PosY) = Action,
    ActionJson = json([type=set, final_x=PosX, final_y=PosY, piece_index=PiecePos]).

% CONVERT TO JSON %
