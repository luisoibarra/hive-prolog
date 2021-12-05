:- module(run_console,[init_game/0,run_game/3]).
:- use_module(run_game_utils).
:- use_module(players).
:- use_module(console_utils).
:- use_module(http_utils).
:- use_module('AI/ai_utils').

% TEST PLAYER
select_player(t, console_human_player, http_game_state, http_game_feedback, http_player_extra_config).
% TEST PLAYER
select_player(0, http_player, http_game_state, http_game_feedback, http_player_extra_config).
select_player(1, console_human_player, print_game_state, print_game_feedback, empty_player_extra_config).
select_player(2, ai_player, print_game_state, print_game_feedback, empty_player_extra_config).
select_player(3, random_player, print_game_state, print_game_feedback, empty_player_extra_config).

initial_game(game([],white,[
    [pieces_info(white,[queen, cricket, cricket, pillbug, ladybug, ant, ant, beetle, beetle, spider, spider]),
     pieces_info(black,[queen, cricket, cricket, pillbug, ladybug, ant, ant, beetle, beetle, spider, spider])],
    [],
    1
])).

init_game() :-
    % Initial game instance
    initial_game(Game),
    % Selecting players
    write('Players:'),nl,
    write('t: HTTP Visual, Console interaction'),nl, % <- TEST PLAYER DELETE
    write('0: HTTP'),nl,
    write('1: Human'),nl,
    write('2: AI'),nl,
    write('3: Random'),nl,
    read_with_headline('Select player 1:', Player1),
    
    % Initializing players
    select_player(Player1, Player1Functor, StartGameStateUserFeedback1, EndGameStateUserFeedback1, Player1ExtraConfigFunctor),
    read_with_headline('Select player 2:', Player2),
    select_player(Player2, Player2Functor, StartGameStateUserFeedback2, EndGameStateUserFeedback2, Player2ExtraConfigFunctor),
    Player1ExtraConfigFun =.. [Player1ExtraConfigFunctor, white, ExtraConfigPlayer1],
    call(Player1ExtraConfigFun),
    Player2ExtraConfigFun =.. [Player2ExtraConfigFunctor, black, ExtraConfigPlayer2],
    call(Player2ExtraConfigFun),

    % Run game
    run_game(Game,
        game_config([
            feedback_info(white, StartGameStateUserFeedback1, EndGameStateUserFeedback1),
            feedback_info(black, StartGameStateUserFeedback2, EndGameStateUserFeedback2)
        ], 
        [
            extra_info(white, ExtraConfigPlayer1), 
            extra_info(black, ExtraConfigPlayer2)
        ]), 
        [player(white, [Player1Functor]), player(black,[Player2Functor])]).

% game([piece(4,3,black,[queen,0]),piece(4,3,white,[beetle,1]),piece(3,2,white,[queen,0])],black,[[pieces_info(white,[cricket,cricket,pillbug,ladybug,ant,ant,beetle,spider,spider]),pieces_info(black,[cricket,cricket,pillbug,ladybug,ant,ant,beetle,beetle,spider,spider])],[],5]), game_config([feedback_info(white,print_game_state,print_game_feedback),feedback_info(black,print_game_state,print_game_feedback)],[extra_info(white,[]),extra_info(black,[])]), [player(white,[console_human_player]),player(black,[console_human_player])]

run_game(Game, GameConfig, Players) :- 

    game_config(GameFeedbackList, ExtraGameConfig) = GameConfig,
    game(Board, CurrentPlayer, [PiecesToSet, GameHistory, Turn|Extra]) = Game,

    % Show Game State
    findall(X, (
        member(feedback_info(PlayerColor, ShowGameStateFunc, _),GameFeedbackList), 
        CallShowGameStateFunc =.. [ShowGameStateFunc, Game, PlayerColor, ExtraGameConfig],
        call(CallShowGameStateFunc)
    ), _),

    not((Board = [], ! ; next_game_step(Game, _))),
    update_game_state(Game, Board, PiecesToSet, NewGame),
    end_turn_feedback(NewGame, Feedback, GameStatus),

    % Play's Feedback
    findall(X, (
        member(feedback_info(PlayerColor, _, ShowGameFeedbackFunc),GameFeedbackList), 
        CallShowGameFeedbackFunc =.. [ShowGameFeedbackFunc, PlayerColor, none, NewGame, Feedback, GameStatus, ExtraGameConfig],
        call(CallShowGameFeedbackFunc)
    ), _),
    
    !,

    write('No valid move for '), write(CurrentPlayer), nl,
    run_game(NewGame, GameConfig, Players).

run_game(Game, GameConfig, Players) :- 
    % Showing State Functions
    game_config(GameFeedbackList, ExtraGameConfig) = GameConfig,
    game(_, CurrentPlayer, _) = Game,
    
    % Show Game State
    findall(X, (
        member(feedback_info(PlayerColor, ShowGameStateFunc, _),GameFeedbackList), 
        CallShowGameStateFunc =.. [ShowGameStateFunc, Game, PlayerColor, ExtraGameConfig],
        call(CallShowGameStateFunc)
    ), _),

    % Player Action Fetcher
    member(player(CurrentPlayer, [PlayerFunctor|_]), Players),
    PlayF =.. [PlayerFunctor, Game, ExtraGameConfig, Action],
    call(PlayF), !,
    
    % Play
    write('Action '), write(CurrentPlayer),nl,
    write(Action),nl,
    write('End Action'),nl,
    make_a_play(Action, Game, NewGame, Feedback, GameStatus),

    write('Game Status'),nl,
    write(GameStatus),nl,
    write('Game Feedback'),nl,
    write(Feedback),nl,
    print_game_state(Game),

    % Play's Feedback
    findall(X, (
        member(feedback_info(PlayerColor, _, ShowGameFeedbackFunc),GameFeedbackList), 
        CallShowGameFeedbackFunc =.. [ShowGameFeedbackFunc, PlayerColor, Action, NewGame, Feedback, GameStatus, ExtraGameConfig],
        call(CallShowGameFeedbackFunc)
    ), _),

    (
        GameStatus = continue,
        run_game(NewGame, GameConfig, Players),
        !
        ;
        member(GameStatus, [over, tie]),
        !,
        initial_game(NewInitialGame),
        run_game(NewInitialGame, GameConfig, Players)
        ;
        run_game(Game, GameConfig, Players)
    ).

run_game(Game, GameConfig, Players) :- 
    write('Some error or invalid play was made, please verify'), nl,
    run_game(Game, GameConfig, Players).