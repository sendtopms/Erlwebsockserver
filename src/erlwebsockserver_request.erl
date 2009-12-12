%% Author: Senthilkumar Peelikkampatti
%% Created: Dec 10, 2009
%% Description: TODO: Add description to erlwebsockserver_request
-module(erlwebsockserver_request, [Socket, Method, RawPath, Version, Headers]).


-include_lib("kernel/include/file.hrl").

-define(QUIP, "Any of you quaids got a smint?").
-define(READ_SIZE, 8192).

-export([send/1, recv/1, recv/2, recv_body/0, recv_body/1]).
-export([start_response/1, start_response_length/1, start_raw_response/1]).
-export([respond/1]).
-export([parse_post/0, parse_qs/0]).
-export([parse_cookie/0, get_cookie_value/1]).

-define(SAVE_QS, mochiweb_request_qs).
-define(SAVE_PATH, mochiweb_request_path).
-define(SAVE_RECV, mochiweb_request_recv).
-define(SAVE_BODY, mochiweb_request_body).
-define(SAVE_BODY_LENGTH, mochiweb_request_body_length).
-define(SAVE_POST, mochiweb_request_post).
-define(SAVE_COOKIE, mochiweb_request_cookie).
-define(SAVE_FORCE_CLOSE, mochiweb_request_force_close).

% 10 second default idle timeout
-define(IDLE_TIMEOUT, 10000).

% Maximum recv_body() length of 1MB
-define(MAX_RECV_BODY, (1024*1024)).



%% @spec send(iodata()) -> ok
%% @doc Send data over the socket.
send(Data) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            ok;
        _ ->
            exit(normal)
    end.

%% @spec recv(integer()) -> binary()
%% @doc Receive Length bytes from the client as a binary, with the default
%%      idle timeout.
recv(Length) ->
    recv(Length, ?IDLE_TIMEOUT).

%% @spec recv(integer(), integer()) -> binary()
%% @doc Receive Length bytes from the client as a binary, with the given
%%      Timeout in msec.
recv(Length, Timeout) ->
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, Data} ->
            put(?SAVE_RECV, true),
            Data;
        _ ->
            exit(normal)
    end.


%% @spec recv_body() -> binary()
%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length of 1MB.
recv_body() ->
    recv_body(?MAX_RECV_BODY).

%% @spec recv_body(integer()) -> binary()
%% @doc Receive the body of the WS request.
%%      Will receive up to MaxBody bytes.
recv_body(MaxBody) ->
    % we could use a sane constant for max chunk size
    Body = "",
%% 		stream_body(?MAX_RECV_BODY, fun
%%         ({0, _ChunkedFooter}, {_LengthAcc, BinAcc}) ->
%%             iolist_to_binary(lists:reverse(BinAcc));
%%         ({Length, Bin}, {LengthAcc, BinAcc}) ->
%%             NewLength = Length + LengthAcc,
%%             if NewLength > MaxBody ->
%%                 exit({body_too_large, chunked});
%%             true ->
%%                 {NewLength, [Bin | BinAcc]}
%%             end
%%         end, {0, []}, MaxBody),
    put(?SAVE_BODY, Body),
    Body.

%% @spec start_response({integer(), ioheaders()}) -> response()
%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders. The server will set header defaults such as Server
%%      and Date if not present in ResponseHeaders.
start_response({Code, ResponseHeaders}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:default_from_list(server_headers(),
                                                    HResponse),
    start_raw_response({Code, HResponse1}).

%% @spec start_raw_response({integer(), headers()}) -> response()
%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders.
start_raw_response({Code, ResponseHeaders}) ->
    F = fun ({K, V}, Acc) ->
                [make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
        end,
    End = lists:foldl(F, [<<"\r\n">>],
                      mochiweb_headers:to_list(ResponseHeaders)),
    send([make_version(Version), make_code(Code), <<"\r\n">> | End]),
    mochiweb:new_response({THIS, Code, ResponseHeaders}).


%% @spec start_response_length({integer(), ioheaders(), integer()}) -> response()
%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders including a Content-Length of Length. The server
%%      will set header defaults such as Server
%%      and Date if not present in ResponseHeaders.
start_response_length({Code, ResponseHeaders, Length}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:enter("Content-Length", Length, HResponse),
    start_response({Code, HResponse1}).

%% @spec respond({integer(), ioheaders(), iodata() | chunked | {file, IoDevice}}) -> response()
%% @doc Start the HTTP response with start_response, and send Body to the
%%      client (if the get(method) /= 'HEAD'). The Content-Length header
%%      will be set by the Body length, and the server will insert header
%%      defaults.
respond({Code, ResponseHeaders, {file, IoDevice}}) ->
%%     Length = iodevice_size(IoDevice),
%%     Response = start_response_length({Code, ResponseHeaders, Length}),
%%     case Method of
%%         'HEAD' ->
%%             ok;
%%         _ ->
%%             iodevice_stream(IoDevice)
%%     end,
%%     Response;
	"";
respond({Code, ResponseHeaders, chunked}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = case Method of
                     'HEAD' ->
                         %% This is what Google does, http://www.google.com/
                         %% is chunked but HEAD gets Content-Length: 0.
                         %% The RFC is ambiguous so emulating Google is smart.
                         mochiweb_headers:enter("Content-Length", "0",
                                                HResponse);
                     _ when Version >= {1, 1} ->
                         %% Only use chunked encoding for HTTP/1.1
                         mochiweb_headers:enter("Transfer-Encoding", "chunked",
                                                HResponse);
                     _ ->
                         %% For pre-1.1 clients we send the data as-is
                         %% without a Content-Length header and without
                         %% chunk delimiters. Since the end of the document
                         %% is now ambiguous we must force a close.
                         put(?SAVE_FORCE_CLOSE, true),
                         HResponse
                 end,
    start_response({Code, HResponse1});
respond({Code, ResponseHeaders, Body}) ->
    Response = start_response_length({Code, ResponseHeaders, iolist_size(Body)}),
    case Method of
        'HEAD' ->
            ok;
        _ ->
            send(Body)
    end,
    Response.

%% @spec parse_qs() -> [{Key::string(), Value::string()}]
%% @doc Parse the query string of the URL.
parse_qs() ->
    case erlang:get(?SAVE_QS) of
        undefined ->
            {_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
            Parsed = mochiweb_util:parse_qs(QueryString),
            put(?SAVE_QS, Parsed),
            Parsed;
        Cached ->
            Cached
    end.

%% @spec get_cookie_value(Key::string) -> string() | undefined
%% @doc Get the value of the given cookie.
get_cookie_value(Key) ->
    proplists:get_value(Key, parse_cookie()).

%% @spec parse_cookie() -> [{Key::string(), Value::string()}]
%% @doc Parse the cookie header.
parse_cookie() ->
   "".

%% @spec parse_post() -> [{Key::string(), Value::string()}]
%% @doc Parse an application/x-www-form-urlencoded form POST. This
%%      has the side-effect of calling recv_body().
parse_post() ->
   "".

%% Internal API

server_headers() ->
    [{"Server", "MochiWeb/1.0 (" ++ ?QUIP ++ ")"},
     {"Date", httpd_util:rfc1123_date()}].

make_version(_) ->
    <<"WS/1.0 Draft">>.

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_code(X) when is_integer(X) ->
    [integer_to_list(X), [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.


