:- module(run_console,[init_game/0,run_game/3]).
:- use_module(run_game_utils).
:- use_module(players).
:- use_module(console_utils).

select_player(1, console_human_player).
select_player(2, ia_player).
select_player(3, random_player).


init_game() :-
    game([],white,[
        [pieces_info(white,[queen, cricket, cricket, cricket, ant, ant, ant, beetle, beetle, spider, spider]),
        pieces_info(black,[queen, cricket, cricket, cricket, ant, ant, ant, beetle, beetle, spider, spider])],
        [],
        1
    ]) = Game,
    write('Players:'),nl,
    write('1: Human'),nl,
    write('2: IA'),nl,
    write('3: Random'),nl,
    read_with_headline('Select player 1:', Player1),
    select_player(Player1, Player1Functor),
    read_with_headline('Select player 2:', Player2),
    select_player(Player2, Player2Functor),
    run_game(Game,
        game_config(print_game_state, print_game_feedback), 
        [player(white, [Player1Functor]), player(black,[Player2Functor])]).

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