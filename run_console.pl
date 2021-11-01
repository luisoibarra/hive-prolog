:- module(run_console,[init_game/0,run_game/1]).
:- use_module(run_game_utils).
:- use_module(players).
:- use_module(console_utils).

init_game() :-
    game([],white,[
        [queen, cricket, cricket, cricket, ant, ant, ant, beetle, beetle, spider, spider],
        [queen, cricket, cricket, cricket, ant, ant, ant, beetle, beetle, spider, spider],
        [],
        1
    ]) = Game,
    run_game(Game, 
        game_config(print_game_state, print_game_feedback), 
        [player(white, [console_human_player]), player(black,[random_player])]).

run_game(Game, GameConfig, Players) :- 
    % Showing State Functions
    game_config(ShowGameStateFunc, ShowGameFeedbackFunc) = GameConfig,
    game(_, CurrentPlayer, _) = Game,
    
    % Show Game State 
    CallShowGameStateFunc =.. [ShowGameStateFunc, Game],
    call(CallShowGameStateFunc),

    % Player Action Fetcher
    member(player(CurrentPlayer, [PlayerFunctor|_]), Players),
    PlayF =.. [PlayerFunctor, Game, Action],
    call(PlayF), !,
    
    % Play
    make_a_play(Action, Game, NewGame, Feedback, GameStatus),
    
    % Play Feedback
    CallShowGameFeedbackFunc =.. [ShowGameFeedbackFunc, Action, NewGame, Feedback, GameStatus],
    call(CallShowGameFeedbackFunc),

    GameStatus = continue,
    run_game(NewGame, GameConfig, Players).

run_game(Game, GameConfig, Players) :- 
    write('Some error or invalid play was made, please verify'), nl,
    run_game(Game, GameConfig, Players).