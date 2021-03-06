:- use_module(minmax).
:- use_module(minmax_utils).

% TESTING MINIMAX
run_minimax_test(R) :-
    two_minimax(step(none, game([piece(4,3,white,[grasshopper,0])],black,[[pieces_info(white,[queen]),pieces_info(black,[queen])],[],2]),none,continue), 0, black, -100000000, 100000000,result_selection,next_step_generator,terminal_test,sample_two_player_utility_function,2,R).

run_minimax_test(R) :-
    % minmax(step(none,game([piece(4,4,white,[queen,0]),piece(3,3,black,[queen,0])],white,[[pieces_info(white,[ant,ant]),pieces_info(black,[ant,ant])],[],1]),none,continue),result_selection,next_step_generator,terminal_test,sample_utility_function,2,R).
    minmax(step(none,game([piece(4,3,white,[grasshopper,0])],black,[[pieces_info(white,[queen]),pieces_info(black,[queen])],[],2]),none,continue),result_selection,next_step_generator,terminal_test,sample_utility_function,2,R).

% game([piece(5,2,black,[queen,0]),piece(4,3,white,[ant,0])],white,[[pieces_info(white,[queen,grasshopper,grasshopper,grasshopper,ant,ant,beetle,beetle,spider,spider]),pieces_info(black,[grasshopper,grasshopper,grasshopper,ant,ant,ant,beetle,beetle,spider,spider])],[],1])
% game([piece(4,3,white,[ant,0])],white,[[pieces_info(white,[queen,grasshopper,grasshopper,grasshopper,ant,ant,beetle,beetle,spider,spider]),pieces_info(black,[grasshopper,grasshopper,grasshopper,ant,ant,ant,beetle,beetle,spider,spider])],[],1])

