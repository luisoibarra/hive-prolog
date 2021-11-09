:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(players).
:- use_module(json_utils).

% Para probar el json
% curl --header "Content-Type: application/json" --request POST --data '{"user":"exy"}' http://localhost:8000/json

:- http_handler('/move', handle_request, [prefix]).
:- http_handler('/player_white', handle_request_game, [prefix]).
:- http_handler('/player_black', handle_request_game, [prefix]).
:- http_handler('/json', handle_json, [prefix]).

handle_json(Request) :-
    http_read_json_dict(Request, Dict),
    reply_json_dict(Dict).

handle_request_game(Request) :-
    http_read_json(Request, JSON),
    json(Properties) = JSON,
    (
        member(action=play,Properties),
        member(player=Player,Properties),
        console_human_player(_,_,Action),
        convert_action_to_json(Action, ActionJson),
        reply_json(ActionJson)
        ;
        % write(Properties),
        reply_json(json([]))
    ).

handle_request(_Request) :-
    format("Content-type: text/plain~n~n"),
    write(_Request),
    format("Move").

% Inicia el servidor
server(Port) :-
    http_server(http_dispatch, [port(Port)]).
