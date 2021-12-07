:- module(console_utils, [print_board/1, print_pieces/1, read_position/2, 
            read_with_headline/2, print_game_state/3, print_game_state/1, 
            print_game_feedback/6, default_if_empty/4, select_option_console/6,
            read_extra_args/2]).
:- use_module(list_utils).

print_board(Board) :- 
    write('Board'), nl,
    write(Board).

print_pieces([]).
print_pieces([pieces_info(ColorPlayer, PiecesToSet)|OtherPlayers]) :- 
    write(ColorPlayer), write(' pieces:'), nl,
    write(PiecesToSet), nl, nl,
    print_pieces(OtherPlayers).

read_position(PosX, PosY) :- 
    read_with_headline('Position X', PosX), 
    read_with_headline('Position Y', PosY). 

read_with_headline(Headline, Read) :- 
    write(Headline), nl,
    read(Read).

read_extra_args(Header, ExtraArgs) :-
    write(Header),nl,
    read_until_no(ExtraArgs).

read_until_no(Values) :-
    write('Write an arg or no for exit'), nl,
    read(ReadValue),
    (
        ReadValue = no,
        Values = []
        ;
        read_until_no(OtherValues),
        Values = [ReadValue|OtherValues]
    ).

default_if_empty(Value, Value, Default, Default) :- !.
default_if_empty(Value, Value2, _, Value) :- Value \= Value2.

% print_game_state(Game, PlayerColor, ExtraGameConfig)
print_game_state(Game, _, _) :- 
    print_game_state(Game).

print_game_state(Game) :- 
    game(Board, CurrentPlayer, [PlayerPieces|_]) = Game,
    print_board(Board), nl,
    print_pieces(PlayerPieces), nl,
    write('Player '), write(CurrentPlayer), write(' turn:'), nl.

% print_game_feedback(PlayerColor, Action, NewGame, Feedback, Status, ExtraGameConfig) :-
print_game_feedback(_, Action, _, Feedback, Status, _) :-
    write(Action),nl,
    write(Status),nl,
    write(Feedback),nl.

select_option_console(Header, ReadHeader, Labels, Options, _, Result) :-
    write(Header),nl,
    zip(Labels, Options, Zipped),
    findall(_,(
        member([Label, Option], Zipped),
        write(Option), write(': '), write(Label), nl
    ),_),
    read_with_headline(ReadHeader, PrevResult),
    (
        member(PrevResult, Options),
        Result = PrevResult, !
        ;
        write('Invalid option. Choose again'), nl,
        !,
        select_option_console(Header, ReadHeader, Labels, Options, _, Result)
    ).