%% Author: Senthilkumar Peelikkampatti
%% Created: Dec 10, 2009
%% Description: TODO: Add description to out_listener
-module(out_listener).
-export ([send_data/2, send_error/1, do_stream/1, close/0]).


send_data (Data, _Header)->
	io:format ("Listener got message ~p~n", [Data]).

send_error (_Info)->
	ok.

do_stream (_)->
	ok.

close ()->
	ok.
