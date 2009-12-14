%%% Copyright 2009 Senthilkumar Peelikkampatti (sendtopms@gmail.com).
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%% @author Senthilkumar Peelikkampatti <sendtopms@gmail.com>
%% @copyright 2009 Senthilkumar Peelikkampatti.

%%% This code is inspired by mochiweb_socket_server and thanks to Bob Ippolito <bob@mochimedia.com>
%%% decode/1 is inspired by Joe Armstrong from the Thread [erlang-questions] web sockets almost working]

-module(erlwebsockserver).
-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-export([hshake/1]).

-export([keep_listening/2]).

-record(erlwebsock_config, {ws_port=8000,
							incoming=erlwebsock_inlistener,
							outgoing= erlwebsock_outlistener,
							max_sock_per_client=2,
							http_host=localhost,
							http_port=8000,
							standalone=false
						   }
	   ).

start(Config=#erlwebsock_config{}) ->
    start_server(Config);
start(Options) ->
    start(parse_options(Options)).

start_server(Config=#erlwebsock_config{standalone=Name, ws_port=WSPort}) ->
	            gen_server:start_link(?MODULE, Config, []).


init(Config=#erlwebsock_config{standalone=Standalone, ws_port=WSPort}) ->
    process_flag(trap_exit, true),
	
    case Standalone of
        true ->
			start_standalone(WSPort);
        _ -> ok
    end,

   {ok, [{config, Config}, {state, []}]}.


handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({listen, Listen}, Config) ->
    spawn(fun() -> keep_listening(Listen, Config) end), 
    {noreply, Config};

handle_cast(stop, State) ->
    {stop, normal, State}.
stop(_) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    State.
handle_info(_, State) ->
    {noreply, State}.
terminate(_, State) ->
    {noreply, State}.

%% Standalone Websocket Server to be started by calling this method. 
%% Need to use Mochiweb socket server 
start_standalone(Config) -> 
    spawn(fun() -> start_listening(Config) end). 

start_listening(Config=#erlwebsock_config{ws_port=Port}) -> 
    {ok, Listen} = gen_tcp:listen(Port, [{packet,0}, 
                                         {reuseaddr,true}, 
                                         {active, false}]), 
    {ok, Socket} = gen_tcp:accept(Listen), 
     spawn(fun() -> keep_listening(Listen, Config) end). 

keep_listening (Listen, Config) ->
     {ok, Socket} = gen_tcp:accept(Listen),
	 gen_server:cast(?MODULE, {listen, Listen}),
 	 communicate(Socket, Config). 
	
communicate(Socket, Config) -> 
     case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            io:format("wait received:~p~n",[Data]), 
			hshake(Socket);
        Any -> 
            io:format("wait Received:~p~n",[Any])
    end. 
hshake(Socket) -> 
            io:format("-----------------hshake-------------------:~n"), 
            Msg = primary_header() ++ 
               next_header("http://localhost:8000", "ws://localhost:8000"), 
            gen_tcp:send(Socket, Msg),
            keep_communicating(Socket).

primary_header() -> 
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n". 

next_header(OriginWithPort, WSLocationWithPort)->
	io_lib:format("WebSocket-Origin: ~s\r\nWebSocket-Location: ~s/\r\n\r\n", [OriginWithPort, WSLocationWithPort]).
	

keep_communicating(Socket) -> 
        case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
			DataFrame = binary_to_list(Data),
			io:format("Websocket Data got is ~p~n", [DataFrame]),
            Data1 = decode(DataFrame), 
            io:format("loop received:~p~n",[Data1]), 
			ToMsg = "Hi This is Erlang based Websocket, I got your missile and it is relaunched back to you at " ++ httpd_util:rfc1123_date(),
            gen_tcp:send(Socket, [0] ++  ToMsg ++ [255]), 
            keep_communicating(Socket); 
        Any -> 
            io:format("loop Received:~p~n",[Any])
%% 			keep_communicating(Socket)
    end. 

%% Internal API

to_atom (L) when is_list(L) ->
	list_to_atom(L);
to_atom (L) -> L.

	
parse_options(Options) ->
    parse_options(Options, #erlwebsock_config{}).

parse_options([], Config) ->
    Config;

parse_options([{ws_port, L} | Rest], Config) when is_list(L) ->
    Port = list_to_integer(L),
    parse_options(Rest, Config#erlwebsock_config{ws_port=Port});
parse_options([{ws_port, Port} | Rest], Config) ->
    parse_options(Rest, Config#erlwebsock_config{ws_port=Port});

parse_options([{incoming, Incoming} | Rest], Config) ->
    parse_options(Rest, Config#erlwebsock_config{incoming=Incoming});

parse_options([{outgoing, Outgoing} | Rest], Config) ->
    parse_options(Rest, Config#erlwebsock_config{outgoing=Outgoing});

parse_options([{max_sock_per_client, Max} | Rest], Config) ->
    MaxInt = case Max of
                 Max when is_list(Max) ->
                     list_to_integer(Max);
                 Max when is_integer(Max) ->
                     Max
             end,
    parse_options(Rest, Config#erlwebsock_config{max_sock_per_client=MaxInt});

parse_options([{http_host, Http_host} | Rest], Config) ->
    parse_options(Rest, Config#erlwebsock_config{http_host=Http_host});
  
parse_options([{standalone, Standalone} | Rest], Config) ->
    parse_options(Rest, Config#erlwebsock_config{standalone=Standalone});
  
parse_options([{http_port, L} | Rest], Config) when is_list(L) ->
    Port = list_to_integer(L),
    parse_options(Rest, Config#erlwebsock_config{http_port=Port});
parse_options([{http_port, Port} | Rest], Config) ->
    parse_options(Rest, Config#erlwebsock_config{http_port=Port}).


decode([0|T]) ->
     decode_next(T);
decode(<<0, T/binary>>) -> decode_next(T). 

decode_next([255]) -> []; 
decode_next([H|T]) -> [H|decode_next(T)];
decode_next(<<255>>) -> <<>>;
decode_next(<<H:8/bitstring, T/bitstring>>) -> Tail = decode_next(T),  << H/binary, Tail/binary>>. 
