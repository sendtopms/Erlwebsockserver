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

-module(erlwebsockserver).
-compile(export_all). 

%% Standalone Websocket Server to be started by calling this method. 
start(Port) -> 
    {ok, Listen} = gen_tcp:listen(Port, [{packet,0}, 
                                         {reuseaddr,true}, 
                                         {active, false}]), 
    spawn(fun() -> keep_listening(Listen) end). 
keep_listening(Listen) -> 
    {ok, Socket} = gen_tcp:accept(Listen), 
    spawn(fun() -> keep_listening(Listen) end), 
    communicate(Socket). 

communicate(Socket) -> 
     case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            io:format("wait received:~p~n",[Data]), 
			hshake(Socket);
        Any -> 
            io:format("wait Received:~p~n",[Any])
%%              communicate(Socket) 
    end. 
hshake(Socket) -> 
            io:format("-----------------hshake-------------------:~n"), 
            Msg = primary_header() ++ 
               next_header("http://localhost:8000", "ws://localhost:8000"), 
            gen_tcp:send(Socket, Msg),
            communicate_forever(Socket).

primary_header() -> 
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n". 

next_header(OriginWithPort, WSLocationWithPort)->
	io_lib:format("WebSocket-Origin: ~s\r\nWebSocket-Location: ~s/\r\n\r\n", [OriginWithPort, WSLocationWithPort]).
	

communicate_forever(Socket) -> 
        case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
			DataFrame = binary_to_list(Data),
			io:format("Websocket Data got is ~p~n", [DataFrame]),
            Data1 = decode(DataFrame), 
            io:format("loop received:~p~n",[Data1]), 
            gen_tcp:send(Socket, [0] ++ "hello from erlang" ++ [255]), 
            communicate_forever(Socket); 
        Any -> 
            io:format("loop Received:~p~n",[Any])
%% 			communicate_forever(Socket)
    end. 

decode([0|T]) ->
     decode_next(T);
decode(<<0, T/binary>>) -> decode_next(T). 

decode_next([255]) -> []; 
decode_next([H|T]) -> [H|decode_next(T)];
decode_next(<<255>>) -> <<>>;
decode_next(<<H:8/bitstring, T/bitstring>>) -> Tail = decode_next(T),  << H/binary, Tail/binary>>. 
