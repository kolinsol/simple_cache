-module(hi_server).

-behaviour(gen_web_server).

-export([start_link/1, start_link/2]).

-export([init/1, get/3, delete/3, head/3, trace/4,
         put/4, post/4, options/4, other_methods/4]).

start_link(Port) ->
    gen_web_server:start_link(?MODULE, Port, []).

start_link(Ip, Port) ->
    gen_web_server:start_link(?MODULE, Ip, Port, []).

init([]) ->
    {ok, []}.

get({http_request, 'GET', {abs_path, <<"/",Key/bytes>>}, _},
    _Head, _UserData) ->
    case simple_cache:lookup(Key) of
        {ok, Value} ->
            gen_web_server:http_reply(200, [], Value);
        {error, not_found} ->
            gen_web_server:http_reply(404, "Sorry, no such key")
    end.

delete({http_request, 'DELETE', {abs_path, <<"/",Key/bytes>>}, _},
       _Head, _UserData) ->
    simple_cache:delete(Key),
    gen_web_server:http_reply(200).

put({http_request, 'PUT', {abs_path, <<"/",Key/bytes>>}, _},
    _Head, Body, _UserData) ->
    simple_cache:insert(Key, Body),
    gen_web_server:http_reply(200).

post(_Request, _Head, _Body, _UserData) ->
    gen_web_server:http_reply(501).

head(_Request, _Head, _UserData) ->
    gen_web_server:http_reply(501).

options(_Request, _Head, _Body, _UserData) ->
    gen_web_server:http_reply(501).

trace(_Request, _Head, _Body, _UserData) ->
    gen_web_server:http_reply(501).

other_methods(_Request, _Head, _Body, _UserData) ->
    gen_web_server:http_reply(501).
