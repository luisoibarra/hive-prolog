:- use_module(minmax).
:- use_module(minmax_utils).

% TESTING MINIMAX
run_minimax_test() :-
    minmax(step(none,game([piece(4,4,white,[queen,0]),piece(3,3,black,[queen,0])],white,[[pieces_info(white,[ant,ant]),pieces_info(black,[ant,ant])],[],1]),none,continue),result_selection,next_step_generator,terminal_test,sample_utility_function,2,R).

