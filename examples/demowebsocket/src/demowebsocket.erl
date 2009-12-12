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





-module(demowebsocket).
-author('Senthilkumar Peelikkampatti <sendtopms@gmail.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the demowebsocket server.
start() ->
    demowebsocket_deps:ensure(),
    ensure_started(crypto),
    application:start(demowebsocket).

%% @spec stop() -> ok
%% @doc Stop the demowebsocket server.
stop() ->
    Res = application:stop(demowebsocket),
    application:stop(crypto),
    Res.
