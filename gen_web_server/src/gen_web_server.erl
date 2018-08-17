-module(gen_web_server).

-export([start_link/3, start_link/4,
         http_reply/1, http_reply/2, http_reply/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1},
     {head, 3},
     {get, 3},
     {delete, 3},
     {options, 4},
     {post, 4},
     {put, 4},
     {trace, 4},
     {other_methods, 4}];
behaviour_info(_Other) ->
    undefined.

start_link(Callbacks, Port, UserArgs) ->
    start_link(Callbacks, undefined, Port, UserArgs).

start_link(Callbacks, Ip, Port, UserArgs) ->
    gws_connection_sup:start_link(Callbacks, Ip, Port, UserArgs).

http_reply(Code, Headers, Body) ->
    ContentBytes = iolist_to_binary(Body),
    Length = byte_size(ContentBytes),
    [io_lib:format("HTTP/1.1 ~s\n~sContent-Length: ~w\n\n",
                   [response(Code), headers(Headers), Length]),
    ContentBytes].

http_reply(Code) ->
    http_reply(Code, <<>>).

http_reply(Code, Body) ->
    http_reply(Code, [{"Content-Type", "text/html"}], Body).

headers([{Header, Text} | Rest]) ->
    [io_lib:format("~s: ~s\n", [Header, Text]) | headers(Rest)];
headers([]) ->
    [].

response(100) -> "100 Continue";
response(200) -> "200 OK";
response(404) -> "404 Not Found";
response(501) -> "501 Not Implemented";
response(Code) -> integer_to_list(Code).
