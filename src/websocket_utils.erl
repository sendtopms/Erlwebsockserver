%% Author: PMS
%% Created: Dec 10, 2009
%% Description: TODO: Add description to websocket_util
-module(websocket_utils).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([is_websocket_request/1, is_websocket/1, adapt_body/1]).

%%
%% API Functions
%%
%% {headers,
%%                       [{'Connection',"Upgrade"},
%%                        {'Cookie',"z_pid=Gkw1M7L9lESuzSVNsY02"},
%%                        {'Host',"localhost:8000"},
%%                        {"Origin","http://localhost:8000"},
%%                        {'Upgrade',"WebSocket"}]}]} Request

is_websocket_request(Request)->
    HShake = get_handshake (Request),
    Headers0 = Request:get(headers),
    Headers = mochiweb_headers:to_list(Headers0),
    io:format("Headers ~p~n", [Headers]),
    Pred = fun({K,V}) -> 
        io:format("K=~p V=~p~n", [K, V]),
                   case proplists:get_value(K, Headers) of
                       V -> true;
                       _ -> false
                   end
           end,
                   
    Result = lists:all(Pred, HShake),
    io:format("Result ~p~n", [Result]),
    Result.
%% 
%% is_websocket (Socket)->
%% false.
is_websocket (Socket)->
	inet:setopts(Socket, [{active, false}, {packet, 0}]),
	case gen_tcp:recv(Socket, 0) of 
		{ok,<<>>} -> 
			io:format("is_websocket received:~p~n",[0]),
			gen_tcp:unrecv(Socket, [0]), 
			true; 
		{ok,H} -> 
			io:format("is_websocket received:~p~n",[H]),
			R= gen_tcp:unrecv(Socket, H), 
			io:format("Ris_websocket received:~p~n",[R]),
			false;
		Error -> 
			io:format("Error is_websocket received:~p~n",[Error]),
			false
	end. 

adapt_body (MochiAppBodyFun)->
		    MochiAppBodyFunAdapter = fun (LReq) ->
                      io:format ("Trapped the Request ~p~n", [LReq]),
					  Isws = websocket_utils:is_websocket_request(LReq),
					  io:format ("is_ws ~p~n", [Isws]),
					  case Isws of
						  true ->
							  io:format("In BodyNext ~n~n"),
%% For Websocket, do nothing --- we will takecare of managing Socket 							  
						  	do_nothing;
						  false ->  
							  	MochiAppBodyFun(LReq)
					  end
					end,
			MochiAppBodyFunAdapter.

%%
%% Local Functions
%%

get_handshake(_Request)->
                   [{'Connection',"Upgrade"},
                    {'Host',"localhost:8081"},
                    {"Origin","http://localhost:8081"},
                    {'Upgrade',"WebSocket"}].
