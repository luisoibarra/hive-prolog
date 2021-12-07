:- module(http_utils,[http_get_action/3, http_game_state/3, 
    http_game_feedback/6, select_option_http/6,
    http_player_extra_config/2]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(json_utils).
:- use_module(console_utils).

send_post(Json, Host, Port, Path, Reply) :- 
    atom_json_term(JsonStr, Json, []),
    Config = [protocol(http),host(Host),port(Port),path(Path)],
    http_post(Config, atom('application/json',JsonStr), Reply, []).

% http_get_action(Game, ExtraGameConfig, Action) Fetch the user action
http_get_action(Game, ExtraGameConfig, Action) :-
    game(_,CurrentPlayer, _) = Game,

    member(extra_info(CurrentPlayer, HTTPInfo), ExtraGameConfig), !,
    member(http_player_config(Host, Port, PrefixPath), HTTPInfo), !,
    send_post(json([action=play, player=CurrentPlayer]), Host, Port, PrefixPath, Reply),
    convert_action_from_json(Reply, Action).

% http_game_state(Game, PlayerColor, ExtraGameConfig) Updates the game status with an HTTP Post
http_game_state(Game, PlayerColor, ExtraGameConfig) :- 
    convert_game_to_json(Game, JsonGame),

    member(extra_info(PlayerColor, HTTPInfo), ExtraGameConfig), !,
    member(http_player_config(Host, Port, PrefixPath), HTTPInfo), !,
    send_post(JsonGame, Host, Port, PrefixPath, _).


% http_game_state(PlayerColor, Action, NewGame, Feedback, Status, ExtraGameConfig) Updates the game status with an HTTP Post
http_game_feedback(PlayerColor, Action, NewGame, Feedback, Status, ExtraGameConfig) :-
    convert_game_to_json(NewGame, GameJson),
    convert_action_to_json(Action, ActionJson),
    json([game=GameJson, action=ActionJson, feedback=Feedback, status=Status]) = FeedbackJson,

    member(extra_info(PlayerColor, HTTPInfo), ExtraGameConfig), !,
    member(http_player_config(Host, Port, PrefixPath), HTTPInfo), !,
    send_post(FeedbackJson, Host, Port, PrefixPath, _).

% http_player_extra_config(ColorPlayer, ExtraGameConfig) Given a player, returns the HTTP configuration
http_player_extra_config(ColorPlayer, ExtraGameConfig) :-
    % write('Player '), write(ColorPlayer), nl,
    % read_with_headline('Host (d if localhost):', HostPrev),
    % default_if_empty(HostPrev, d, '127.0.0.1', Host),
    % read_with_headline('Port (d if 9001):', PortPrev),
    % default_if_empty(PortPrev, d, 9001, Port),
    concat('/player_', ColorPlayer, PrefixPath),
    Host = '127.0.0.1', Port = 9001, 
    ExtraGameConfig = [http_player_config(Host, Port, PrefixPath)].

select_option_http(Header, ReadHeader, Labels, Options, HTTPInfo, Result) :-
    convert_question_to_json(Header, ReadHeader, Labels, Options, QuestionJson),
    
    % member(game(_, CurrentPlayer, _), ExtraGameConfig), !,
    % member(extra_info(_, HTTPInfo), ExtraGameConfig), !,
    member(http_player_config(Host, Port, PrefixPath), HTTPInfo), !,
    send_post(QuestionJson, Host, Port, PrefixPath, Reply),

    convert_question_response_from_json(Reply, Result).