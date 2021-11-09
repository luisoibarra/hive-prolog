:- module(http_utils,[http_get_action/3, http_game_state/3, 
    http_game_feedback/6]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(json_utils).

send_post(Json, Host, Port, Path, Reply) :- 
    atom_json_term(JsonStr, Json, []),
    Config = [protocol(http),host(Host),port(Port),path(Path)],
    http_post(Config, atom('application/json',JsonStr), Reply, []),
    write(Reply).

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
