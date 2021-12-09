:- module(utility_function,[player_utility_function/3, piece_to_advantage/4, piece_to_danger/4]).
:- use_module('../Run/run_game_utils').
:- use_module('../Rules/end_game_rules').
:- use_module('../Rules/move_piece_rules').
:- use_module('../Utils/list_utils').
:- use_module('../Utils/piece_utils').
:- use_module('../Utils/board_utils').
:- use_module(ai_utils).

player_utility_function(State, UtilityPlayer, Vector) :- 
    step(Action,_, _, _) = State,
    priv_player_utility_function(State, UtilityPlayer, Value),
    Vector = [Action, Value].

% priv_player_utility_function(State, Player, Value) Returns in Value the utility value for Player given the State
priv_player_utility_function(State, UtilityPlayer, Value) :-
    lost_game(State, UtilityPlayer),
    Value = -100, !.

priv_player_utility_function(State, UtilityPlayer, Value) :-
    won_game(State, UtilityPlayer),
    Value = 100, !.

priv_player_utility_function(State, _, Value) :-
    tie_game(State),
    Value = 0, !.

priv_player_utility_function(State, UtilityPlayer, Score) :-
    step(_, Game, _, _) = State,
    game(Board, _, _) = Game,
    % all_next_game_steps(Game, Steps),
    
    % unification_filter(Steps, step(set_play(_,_,_),_,_,_), SetSteps),
    % length(SetSteps, SetStepsLength),
    
    % unification_filter(Steps, step(move_play(_,_,_,_,_),_,_,_), MoveSteps),
    % length(MoveSteps, MoveStepsLength),

    moveable_pieces(Game, MoveablePieces),
    findall(X, (
        member(X, MoveablePieces), 
        piece(_, _, Player, _) = X, 
        Player \= UtilityPlayer), 
    MoveEnemies),
    length(MoveEnemies, MoveEnemiesLength),

    findall(X, (
        member(X, MoveablePieces), 
        piece(_, _, Player, _) = X, 
        Player = UtilityPlayer), 
    MoveAllied),
    length(MoveAllied, MoveAlliedLength),

    length(MoveablePieces, AllMovesLength), 
    
    % write('pieces: '), write(MoveablePieces), nl, % DEBUG 
    % write('total: '), write(AllMovesLength), nl, 
    % write('allied: '), write(MoveAlliedLength), nl,
    % write('enemies: '), write(MoveEnemiesLength), nl,

    board_pieces_danger(State, UtilityPlayer, PiecesPlacedDanger),
    
    board_pieces_advantage(State, UtilityPlayer, PiecesPlacedAdvantange),
    
    enemy_queen_threat(State, UtilityPlayer, EnemyQueenThreat),
    
    allied_queen_threat(State, UtilityPlayer, AlliedQueenThreat),
    
    normalize_score_function(State, 
        [
            % SetStepsLength, 
            % MoveStepsLength, 
            MoveEnemiesLength,
            MoveAlliedLength,
            AllMovesLength,
            PiecesPlacedDanger, 
            PiecesPlacedAdvantange,
            EnemyQueenThreat,
            AlliedQueenThreat
        ], Score).

% lost_game(State, UtilityPlayer) Succeed if UtilityPlayer lost the game 
lost_game(State, UtilityPlayer) :- 
    step(_, Game, _, _) = State,
    game(Board,_,_) = Game,
    queen_surrounded(Board, UtilityPlayer).

% won_game(State, UtilityPlayer) Succeed if UtilityPlayer won the game 
won_game(State, UtilityPlayer) :- 
    step(_, Game, _, _) = State,
    get_game_Players(Game, Players),
    list_difference(Players, [UtilityPlayer], OtherPlayers),
    findall(P, (member(P, OtherPlayers), lost_game(State, P)), Lost),
    length(Lost, LengthLost),
    length(OtherPlayers, LengthOtherPlayers),
    LengthLost = LengthOtherPlayers.
    
% tie_game(State, UtilityPlayer) Succeed the game is a tie. Assumes that there is no winner 
tie_game(State) :-
    step(_, Game, _, _) = State,
    not(next_game_step(Game, _)).

% piece_danger_points(Bug, Danger) Returns the Danger given a Bug
% max Danger allowed is 50
% TODO Check Danger Values
piece_danger_points(spider, 15) :- !.
piece_danger_points(queen, 10) :- !.
piece_danger_points(beetle, 30) :- !.
piece_danger_points(cricket, 25) :- !.
piece_danger_points(ant, 30) :- !.
piece_danger_points(ladybug, 25) :- !.
piece_danger_points(mosquito, 30) :- !.
piece_danger_points(pillbug, 25) :- !.
piece_danger_points(_, 10) :- !.

% TODO Use Turn to add more advantage to pieces in different game stages
piece_to_danger(UtilityPlayer, Turn, Piece, Danger) :-
    piece(_,_,Player,_) = Piece,
    (
        Player = UtilityPlayer,
        Danger = 0, !
        ;
        Player \= UtilityPlayer,
        get_piece_Type(Piece, Type),
        piece_danger_points(Type, Danger)
    ).

is_movable(Board, Piece) :-
    move(Board, Piece, _, _, _, _, _), !.

% moveable_pieces(Game, MoveablePieces) Returns the MoveablePieces from Game
moveable_pieces(Game, MoveablePieces) :- 
    game(Board, _, _) = Game,
    findall(X, (
        member(X, Board), 
        is_movable(Board, X)), 
    MoveablePieces).

% board_pieces_danger(State, UtilityPlayer, PiecesPlacedDangers) Returns the Danger of the pieces placed on the board
% PiecesPlacedDangers is a number between 0 and 50
board_pieces_danger(State, UtilityPlayer, PiecesPlacedDangers) :-
    step(_, Game, _, _) = State,
    game(Board, _, _) = Game,
    get_game_Turn(Game, Turn),
    unification_filter(Board, piece(_,_,UtilityPlayer, _), PlayerBoard),
    list_difference(Board, PlayerBoard, NoPlayerBoard),
    map(piece_to_danger, [UtilityPlayer, Turn], NoPlayerBoard, IndividualDangers),
    sum_list(IndividualDangers, SumPiecesPlacedDangers),
    length(IndividualDangers, Length),
    (
        Length = 0,
        PiecesPlacedDangers = 0
        ;
        Length \= 0,
        PiecesPlacedDangers is SumPiecesPlacedDangers / Length
    ).

% TODO Use Turn to add more advantage to pieces in different game stages
piece_to_advantage(UtilityPlayer, Turn, Piece, Advantage) :-
    piece(_,_,Player,_) = Piece,
    (
        Player \= UtilityPlayer,
        Advantage = 0, !
        ;
        Player = UtilityPlayer,
        get_piece_Type(Piece, Type),
        piece_danger_points(Type, Advantage)
    ).

% board_pieces_advantage(State, UtilityPlayer, PiecesPlacedAdvantage) Returns the Advantage of the pieces placed on the board
% PiecesPlacedAdvantage is a number between 0 and 50
board_pieces_advantage(State, UtilityPlayer, PiecesPlacedAdvantage) :- 
    step(_, Game, _, _) = State,
    game(Board, _, _) = Game,
    get_game_Turn(Game, Turn),
    unification_filter(Board, piece(_,_,UtilityPlayer, _), PlayerBoard),
    map(piece_to_advantage, [UtilityPlayer, Turn], PlayerBoard, IndividualAdvantages),
    sum_list(IndividualAdvantages, SumPiecesPlacedAdvantage),
    length(IndividualAdvantages, Length),
    (
        Length = 0,
        PiecesPlacedAdvantage = 0
        ;
        Length \= 0,
        PiecesPlacedAdvantage is SumPiecesPlacedAdvantage / Length
    ).

% enemy_queen_threat(State, UtilityPlayer, EnemyQueenThreat) Returns the Threat level of the enemy queen
% EnemyQueenThreat is a number between 0 and 8 give or take
enemy_queen_threat(State, UtilityPlayer, EnemyQueenThreat) :-
    step(_, Game, _, _) = State,
    game(Board, _, _) = Game,

    get_game_Players(Game, Players),
    list_difference(Players, [UtilityPlayer], OtherPlayers),
    findall(X, 
        (
            member(Player, OtherPlayers), 
            queen_surrounding_pieces(Board, Player, Pieces), 
            length(Pieces, X)
        ), QueenEnemyThreats),

    sum_list(QueenEnemyThreats, SumThreats),
    length(OtherPlayers, LengthOtherPlayers),
    
    EnemyQueenThreat is SumThreats/LengthOtherPlayers.

% allied_queen_threat(State, UtilityPlayer, AlliedQueenThreat) Returns the Threat level of the allied queen
% AlliedQueenThread is a number between 0 and 8 give or take
allied_queen_threat(State, UtilityPlayer, AlliedQueenThreat) :- 
    step(_, Game, _, _) = State,
    game(Board, _, _) = Game,
    queen_surrounding_pieces(Board, UtilityPlayer, Pieces),
    length(Pieces, AlliedQueenThreat).
    
% normalize_score_function(State, Metrics, Score) Combines Metrics to give final Score 
normalize_score_function(State, Metrics, Score) :- 
    
    Metrics = [
        % SetStepsLength, % Amount of piece enemies placements 0 <= x < ? Can be medium apprx 85 +-
        % MoveStepsLength, % Amount of piece enemies movements 0 <= x < ? Can be big apprx 100
        MoveEnemiesLength,
        MoveAlliedLength,
        AllMovesLength,
        PiecesPlacedDanger, % Danger level of placed enemy pieces 0 <= x <= 50
        PiecesPlacedAdvantange, % Andvantage level of placed allied pieces 0 <= x <= 50
        EnemyQueenThreat, % Enemy Queen Thread Level 0 <= x <= +-8
        AlliedQueenThreat % Allied Queen Thread Level 0 <= x <= +-8
        |_
    ],
    step(_, Game, _, _) = State,
    get_game_Turn(Game, Turn),

    % TODO Find Better Params
    % write('PiecesPlacedAdvantange '), write(PiecesPlacedAdvantange), % DEBUG
    % game(Board, _, _) = Game,
    % write('Board '), write(Board), % DEBUG
    Params = [
        % [SetStepsLength,        -1/85, -1/25, 0, Turn, NormalizedSetStepsLength],
        % [MoveStepsLength,       1/100, -1/10,-1, Turn, NormalizedMoveStepsLength],
        % [PiecesPlacedDanger,    -1/50, -1/25, 0, Turn, NormalizedPiecesPlacedDanger],
        % [PiecesPlacedAdvantange, 1/50, -1/30, 0, Turn, NormalizedPiecesPlacedAdvantange],
        [MoveEnemiesLength, -1/AllMovesLength, 0, 0, Turn, NormalizedMoveEnemiesLength],
        [MoveAlliedLength,   1/AllMovesLength, 0, 0, Turn, NormalizedMoveAlliedLength],
        [PiecesPlacedDanger,    -1/50,     -1/40, 0, Turn, NormalizedPiecesPlacedDanger],
        [PiecesPlacedAdvantange, 2/50,     -1/30, 0, Turn, NormalizedPiecesPlacedAdvantange],
        [EnemyQueenThreat,       2.5/8,        0, 0, Turn, NormalizedEnemyQueenThreat],
        [AlliedQueenThreat,     -2.5/8,        0, 0, Turn, NormalizedAlliedQueenThreat]        
    ],

    findall(X, (
        member(P, Params),
        last(P, X),
        concat_list([normalize], P, FunParams),
        F =.. FunParams,
        call(F)
    ), Normalized),

    write(Normalized),nl, % DEBUG
    sum_list(Normalized, Score).
    % sum_list(Normalized, Score). TODO Maybe is better to return the value with highest abs value? 


% normalize(Value, MultParam, ExpParam, Turn, NormalizedResult) 
% NormalizedResult = Value * MultParam * (3 ** (ExpParam * Turn) + SumValue) 
normalize(Value, MultParam, ExpParam, SumValue, Turn, NormalizedResult) :- 
    Arg is Turn * ExpParam,
    Mod is 3 ** Arg + SumValue,
    Mod2 is MultParam * Mod,
    RawNormalizedResult is Mod2 * Value,
    NormalizedResult = RawNormalizedResult.
    % (
    %     RawNormalizedResult < 0,
    %     NormalizedResult is max(RawNormalizedResult, -1)
    %     ;
    %     RawNormalizedResult >= 0,
    %     NormalizedResult is min(RawNormalizedResult, 1)
    % ).