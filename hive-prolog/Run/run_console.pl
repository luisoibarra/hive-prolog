:- module(run_console,[init_game/0,run_game/3]).
:- use_module(run_game_utils).
:- use_module(players).
:- use_module('../Utils/console_utils').
:- use_module('../Utils/list_utils').
:- use_module('../HTTP/http_utils').
:- use_module('../AI/ai_utils').

% PLAYERS CONFIGURATIONS

% TEST PLAYER
select_player(t, console_human_player, http_game_state, http_game_feedback, http_player_extra_config).
% TEST PLAYER
select_player(0, http_player, http_game_state, http_game_feedback, http_player_extra_config).
select_player(1, console_human_player, print_game_state, print_game_feedback, empty_player_extra_config).
select_player(2, ai_player, http_game_state, http_game_feedback, empty_player_extra_config).
select_player(3, random_player, http_game_state, http_game_feedback, empty_player_extra_config).

select_player('0', http_player, http_game_state, http_game_feedback, http_player_extra_config).
select_player('1', console_human_player, print_game_state, print_game_feedback, empty_player_extra_config).
select_player('2', ai_player, http_game_state, http_game_feedback, empty_player_extra_config).
select_player('3', random_player, http_game_state, http_game_feedback, empty_player_extra_config).


% CONSOLE VERSIONS
% select_player(2, ai_player, print_game_state, print_game_feedback, empty_player_extra_config).
% select_player(3, random_player, print_game_state, print_game_feedback, empty_player_extra_config).

% INITIAL HIVE PIECES
pieces_for_hive(c, [queen, cricket, cricket, cricket, beetle, beetle, spider, spider, ant, ant, ant], 'Classic').
pieces_for_hive(e, Pieces, 'Extended') :- 
    pieces_for_hive(c, ClassicPieces, _),
    concat_list(ClassicPieces, [mosquito, ladybug, pillbug], Pieces).
pieces_for_hive(_, Pieces, 'Defaults to Classic') :-
    pieces_for_hive(c, Pieces, _).

% Initial Interface
% interface_functor(select_option_console).
interface_functor(select_option_http).

% Initial config setup
% default_game_config(empty_player_extra_config).
default_game_config(http_player_extra_config).

initial_game(Game, ExtraGameConfig) :- 
    Game = game([],white,[
        [pieces_info(white,Pieces),
         pieces_info(black,Pieces)],
        [],
        1
    ]),
    interface_functor(Functor),
    
    InterfaceFunction =.. [Functor, 'Hive Version', 'Select Mode:', 
        ['Classic', 'Expanded'], 
        ['c','e'], ExtraGameConfig, Version],
    call(InterfaceFunction),

    pieces_for_hive(Version, Pieces, Name), !,
    write('Selected: '), write(Name), nl.

get_default_extra_config(DefaultExtraConfig) :- 
    % Base interface configuration
    default_game_config(DefaultExtraConfigFunctor),
    DefaultExtraConfigFun =.. [DefaultExtraConfigFunctor, none, DefaultExtraConfig],
    call(DefaultExtraConfigFun).

init_game() :-
    get_default_extra_config(DefaultExtraConfig),

    % Initial game instance
    initial_game(Game, DefaultExtraConfig),
    
    interface_functor(Functor),
    % Selecting players
    Player1InterfaceFunction =.. [Functor, 'Players', 'Select white player:', 
        ['Human', 'AI', 'Random'], 
        [0,2,3], DefaultExtraConfig, Player1],
    call(Player1InterfaceFunction),
    Player2InterfaceFunction =.. [Functor, 'Players', 'Select black player:', 
    ['Human', 'AI', 'Random'], 
    [0,2,3], DefaultExtraConfig, Player2],
    call(Player2InterfaceFunction),

    % Initializing players
    select_player(Player1, Player1Functor, StartGameStateUserFeedback1, EndGameStateUserFeedback1, Player1ExtraConfigFunctor),
    select_player(Player2, Player2Functor, StartGameStateUserFeedback2, EndGameStateUserFeedback2, Player2ExtraConfigFunctor),
    
    Player1ExtraConfigFun =.. [Player1ExtraConfigFunctor, white, ExtraConfigPlayer1],
    call(Player1ExtraConfigFun),
    Player2ExtraConfigFun =.. [Player2ExtraConfigFunctor, black, ExtraConfigPlayer2],
    call(Player2ExtraConfigFun),

    ExtraGameConfig = [
        extra_info(white, ExtraConfigPlayer1), 
        extra_info(black, ExtraConfigPlayer2)
    ],

    % Run game
    run_game(Game,
        game_config([
            feedback_info(white, StartGameStateUserFeedback1, EndGameStateUserFeedback1),
            feedback_info(black, StartGameStateUserFeedback2, EndGameStateUserFeedback2)
        ],
        ExtraGameConfig 
        ), 
        [player(white, [Player1Functor]), player(black,[Player2Functor])]).

% game([piece(4,3,black,[queen,0]),piece(4,3,white,[beetle,1]),piece(3,2,white,[queen,0])],black,[[pieces_info(white,[cricket,cricket,pillbug,ladybug,ant,ant,beetle,spider,spider]),pieces_info(black,[cricket,cricket,pillbug,ladybug,ant,ant,beetle,beetle,spider,spider])],[],5]), game_config([feedback_info(white,print_game_state,print_game_feedback),feedback_info(black,print_game_state,print_game_feedback)],[extra_info(white,[]),extra_info(black,[])]), [player(white,[console_human_player]),player(black,[console_human_player])]

run_game(Game, GameConfig, Players) :- 

    game_config(GameFeedbackList, ExtraGameConfig) = GameConfig,
    game(Board, CurrentPlayer, [PiecesToSet|_]) = Game,

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
    print_game_state(NewGame),

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
        get_default_extra_config(DefaultExtraConfig),
        initial_game(NewInitialGame, DefaultExtraConfig),
        run_game(NewInitialGame, GameConfig, Players)
        ;
        run_game(Game, GameConfig, Players)
    ).

run_game(Game, GameConfig, Players) :- 
    write('Some error or invalid play was made, please verify'), nl,
    run_game(Game, GameConfig, Players).