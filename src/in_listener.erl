%% Author: Senthilkumar Peelikkampatti
%% Created: Dec 10, 2009
%% Description: TODO: Add description to in_listener
-module(in_listener).
-export ([on_data/2, on_error/1, on_stream/1, get_config/0, get_listening_channel/0]).


on_data (Data, _Header)->
	io:format ("Listener got message ~p~n", [Data]).

on_error (_Info)->
	ok.

on_stream (_)->
	ok.

get_config()->
	ok.

get_listening_channel()->
	ok.
	
