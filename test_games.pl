:- use_module(run_game_utils).
   
% test_games(TestName, Game, Movements, ExpectedResults) :- 
test_games('Set Queens',Game, Movements, ExpectedResults) :- 
    game(Board, CurrentPlayer, [PiecesToSet, GameHistory, Turn|_]) = Game,
    Board = [],
    CurrentPlayer = white,
    WhiteTypePieces = pieces_info(white,[queen, ant, ant, ant]),
    BlackTypePieces = pieces_info(black,[queen, ant, ant, ant]),
    PiecesToSet = [WhiteTypePieces, BlackTypePieces],
    GameHistory = [],
    Turn = 1,
    Movements = [set_play(0, 3, 3), set_play(0, 3, 2)],
    ExpectedResults = [continue, continue].


test_games('Fail Set Queens',Game, Movements, ExpectedResults) :- 
    game(Board, CurrentPlayer, [PiecesToSet, GameHistory, Turn|_]) = Game,
    Board = [],
    CurrentPlayer = white,
    WhiteTypePieces = pieces_info(white,[queen, ant, ant, ant]),
    BlackTypePieces = pieces_info(black,[queen, ant, ant, ant]),
    PiecesToSet = [WhiteTypePieces, BlackTypePieces],
    GameHistory = [],
    Turn = 1,
    Movements = [set_play(0, 3, 3), set_play(0, 4, 2)],
    ExpectedResults = [continue, invalid].

test_games('Place queen before 4th round', Game, Movements, ExpectedResults) :- 
    game(Board, CurrentPlayer, [PiecesToSet, GameHistory, Turn|_]) = Game,
    Board = [],
    CurrentPlayer = white,
    WhiteTypePieces = pieces_info(white,[ant, ant, ant, ant, ant]),
    BlackTypePieces = pieces_info(black,[ant, ant, ant, ant, ant]),
    PiecesToSet = [WhiteTypePieces, BlackTypePieces],
    GameHistory = [],
    Turn = 1,
    Movements = [set_play(0, 3, 3), set_play(0, 4, 3),
                 set_play(0, 2, 4), set_play(0, 5, 2),
                 set_play(0, 1, 4), set_play(0, 6, 2),
                 set_play(0, 0, 5)],
    ExpectedResults = [continue, continue,
                       continue, continue,
                       continue, continue,
                       invalid].

test_games('Move when queen is placed', Game, Movements, ExpectedResults) :- 
    game(Board, CurrentPlayer, [PiecesToSet, GameHistory, Turn|_]) = Game,
    Board = [],
    CurrentPlayer = white,
    WhiteTypePieces = pieces_info(white,[ant, ant, ant, queen, ant]),
    BlackTypePieces = pieces_info(black,[ant, ant, ant, queen, ant]),
    PiecesToSet = [WhiteTypePieces, BlackTypePieces],
    GameHistory = [],
    Turn = 1,
    Movements = [set_play(0, 3, 3), set_play(0, 4, 3),
                 move_play(3, 3, 4, 2)],
    ExpectedResults = [continue, continue,
                       invalid].


test_games('Queen surrounded', Game, Movements, ExpectedResults) :- 
    game(Board, CurrentPlayer, [PiecesToSet, GameHistory, Turn|_]) = Game,
    Board = [],
    CurrentPlayer = white,
    WhiteTypePieces = pieces_info(white,[ant, queen, ant, ant, ant, ant]),
    BlackTypePieces = pieces_info(black,[ant, queen, ant, ant, ant]),
    PiecesToSet = [WhiteTypePieces, BlackTypePieces],
    GameHistory = [],
    Turn = 1,
    Movements = [set_play(0, 3, 3), set_play(0, 4, 3),
                 set_play(0, 2, 4), set_play(0, 5, 2),
                 set_play(0, 1, 4), set_play(0, 6, 2),
                 set_play(0, 0, 5), set_play(0, 7, 1),
                 move_play(0, 5, 2, 3), move_play(7, 1, 1, 3),
                 set_play(0, 2, 5), move_play(6, 2, 3, 4)],
    ExpectedResults = [continue, continue,
                       continue, continue,
                       continue, continue,
                       continue, continue,
                       continue, continue,
                       continue, over].
                    
simulate_game_test(TestName, _, [], _) :- 
    write('SUCCEED '), write(TestName), !. 

simulate_game_test(TestName, Game, [Move|Movements], [ExpectedGameStatus|GameStatuses]) :- 
    make_a_play(Move, Game, NewGame, Feedback, GameStatus),!,
    (
        GameStatus = ExpectedGameStatus,
        simulate_game_test(TestName, NewGame, Movements, GameStatuses),
        !
        ;
        get_game_Turn(Game, Turn),
        write('FAIL '), write(TestName), write(' '), write(Feedback), write(' '), write(' at turn '), write(Turn), nl,
        write('Received '), write(GameStatus), nl,
        write('Expected '), write(ExpectedGameStatus), nl
    ).

simulate_game_test(TestName, Game, _, _) :- 
    % If HERE make_play FAILED
    get_game_Turn(Game, Turn),
    write('FAIL '), write(TestName), write(' at turn '), write(Turn), nl.


run_game_tests() :-
    test_games(TestName, Game, Movements, ExpectedResults),
    write(TestName), nl,nl,
    simulate_game_test(TestName, Game, Movements, ExpectedResults).
