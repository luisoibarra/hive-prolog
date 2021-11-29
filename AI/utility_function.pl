:- module(utility_function,[player_utility_function/2, piece_to_advantage/4, piece_to_danger/4]).
:- use_module('../run_game_utils').
:- use_module('../game_rules').
:- use_module('../list_utils').
:- use_module('../piece_utils').
:- use_module('../board_utils').
:- use_module(ai_utils).

player_utility_function(State, Vector) :- 
    step(Action,Game, _, _) = State,
    game(_,Player, _) = Game,
    player_utility_function(State, Player, Value),
    Vector = [Action, Value].

% player_utility_function(State, Player, Value) Returns in Value the utility value for Player given the State
player_utility_function(State, UtilityPlayer, Value) :-
    lost_game(State, UtilityPlayer),
    Value = -100, !.

player_utility_function(State, UtilityPlayer, Value) :-
    won_game(State, UtilityPlayer),
    Value = 100, !.

player_utility_function(State, _, Value) :-
    tie_game(State),
    Value = 0, !.

player_utility_function(State, UtilityPlayer, Score) :-
    step(_, Game, _, _) = State,
    all_next_game_steps(Game, Steps),
    
    unification_filter(Steps, step(set_play(_,_,_),_,_,_), SetSteps),
    length(SetSteps, SetStepsLength),
    
    unification_filter(Steps, step(move_play(_,_,_,_),_,_,_), MoveSteps),
    length(MoveSteps, MoveStepsLength),

    board_pieces_danger(State, UtilityPlayer, PiecesPlacedDanger),
    
    board_pieces_advantage(State, UtilityPlayer, PiecesPlacedAdvantange),
    
    enemy_queen_threat(State, UtilityPlayer, EnemyQueenThreat),
    
    allied_queen_threat(State, UtilityPlayer, AlliedQueenThreat),
    
    normalize_score_function(State, 
        [
            SetStepsLength, 
            MoveStepsLength, 
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
piece_danger_points(spider, 20) :- !.
piece_danger_points(queen, 15) :- !.
piece_danger_points(beetle, 30) :- !.
piece_danger_points(cricket, 25) :- !.
piece_danger_points(ant, 30) :- !.
piece_danger_points(ladybug, 30) :- !.
piece_danger_points(mosquito, 30) :- !.
piece_danger_points(pillbug, 30) :- !.
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


% board_pieces_danger(State, UtilityPlayer, PiecesPlacedDangers) Returns the Danger of the pieces placed on the board
% PiecesPlacedDangers is a number between 0 and 50
board_pieces_danger(State, UtilityPlayer, PiecesPlacedDangers) :-
    step(_, Game, _, _) = State,
    game(Board, _, _) = Game,
    get_game_Turn(Game, Turn),
    map(piece_to_danger, [UtilityPlayer, Turn], Board, IndividualDangers),
    sum_list(IndividualDangers, SumPiecesPlacedDangers),
    length(IndividualDangers, Length),
    PiecesPlacedDangers is SumPiecesPlacedDangers / Length.

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
    map(piece_to_advantage, [UtilityPlayer, Turn], Board, IndividualAdvantages),
    sum_list(IndividualAdvantages, SumPiecesPlacedAdvantage),
    length(IndividualAdvantages, Length),
    PiecesPlacedAdvantage is SumPiecesPlacedAdvantage / Length.

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
        SetStepsLength, % Amount of piece enemies placements 0 <= x < ? Can be medium apprx 20
        MoveStepsLength, % Amount of piece enemies movements 0 <= x < ? Can be big apprx 100
        PiecesPlacedDanger, % Danger level of placed enemy pieces 0 <= x <= 50
        PiecesPlacedAdvantange, % Andvantage level of placed allied pieces 0 <= x <= 50
        EnemyQueenThreat, % Enemy Queen Thread Level 0 <= x <= +-8
        AlliedQueenThreat % Allied Queen Thread Level 0 <= x <= +-8
        |_
    ],
    step(_, Game, _, _) = State,
    get_game_Turn(Game, Turn),

    % TODO Find Better Params

    normalize(SetStepsLength, -1/20, 0, Turn, NormalizedSetStepsLength), 
    normalize(MoveStepsLength, -1/50, 0, Turn, NormalizedMoveStepsLength), 
    normalize(PiecesPlacedDanger, -1/50, 0, Turn, NormalizedPiecesPlacedDanger), 
    normalize(PiecesPlacedAdvantange, 1/50, 0, Turn, NormalizedPiecesPlacedAdvantange), 
    normalize(EnemyQueenThreat, 2/8, 0, Turn, NormalizedEnemyQueenThreat), 
    normalize(AlliedQueenThreat, -1/8, 0, Turn, NormalizedAlliedQueenThreat),

    sum_list([
            NormalizedSetStepsLength,
            NormalizedMoveStepsLength,
            NormalizedPiecesPlacedDanger,
            NormalizedPiecesPlacedAdvantange,
            NormalizedEnemyQueenThreat,
            NormalizedAlliedQueenThreat
    ], Score).


% normalize(Value, MultParam, ExpParam, Turn, NormalizedResult) 
% NormalizedResult = Value * MultParam * 3 ** (ExpParam * Turn) 
normalize(Value, MultParam, ExpParam, Turn, NormalizedResult) :- 
    Arg is Turn * ExpParam,
    Mod is 3 ** Arg,
    Mod2 is MultParam * Mod,
    NormalizedResult is Mod2 * Value.