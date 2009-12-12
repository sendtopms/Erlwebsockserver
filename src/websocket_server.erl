%% Author: PMS
%% Created: Dec 10, 2009
%% Description: TODO: Add description to local_server
-module(websocket_server).
-compile(export_all). 
start() -> 
    {ok, Listen} = gen_tcp:listen(8081, [{packet,0}, 
                                         {reuseaddr,true}, 
                                         {active, false}]), 
    spawn(fun() -> par_connect(Listen) end). 
par_connect(Listen) -> 
    {ok, Socket} = gen_tcp:accept(Listen), 
    spawn(fun() -> par_connect(Listen) end), 
    wait(Socket). 
wait(Socket) -> 
     case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            io:format("wait received:~p~n",[Data]), 
			hshake(Socket);
        Any -> 
            io:format("wait Received:~p~n",[Any])
%%              wait(Socket) 
    end. 
hshake(Socket) -> 
            io:format("-----------------hshake-------------------:~n"), 
            Msg = prefix() ++ 
                "WebSocket-Origin: http://localhost:8081\r\n" ++ 
                "WebSocket-Location: ws://localhost:8081/\r\n\r\n", 
            io:format("Hshake message :~p~n",[Msg]), 
            gen_tcp:send(Socket, Msg),
            loop(Socket).
prefix() -> 
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n". 
loop(Socket) -> 
        case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
			DataFrame = binary_to_list(Data),
			io:format("Websocket Data got is ~p~n", [DataFrame]),
            Data1 = unframe(DataFrame), 
            io:format("loop received:~p~n",[Data1]), 
            gen_tcp:send(Socket, [0] ++ "hello from erlang" ++ [255]), 
            loop(Socket); 
        Any -> 
            io:format("loop Received:~p~n",[Any])
%% 			loop(Socket)
    end. 
loop0(Socket) -> 
		inet:setopts(Socket, [{active, false}, {packet,0}]),
        case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            Data1 = unframe(Data), 
            io:format("loop received:~p~n",[Data1]), 
            gen_tcp:send(Socket, [0] ++ "hello from erlang" ++ [255]), 
            loop(Socket); 
        Any -> 
            io:format("loop Received:~p~n",[Any])
    end. 

unframe([0|T]) -> unframe1(T). 
unframe1([255]) -> []; 
unframe1([H|T]) -> [H|unframe1(T)]. 