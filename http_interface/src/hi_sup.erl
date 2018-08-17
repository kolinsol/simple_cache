-module(hi_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

start_child() ->
    supervisor:start_child(?SERVER, []).

init([Port]) ->
     HttpServer = {hi_server, {hi_server, start_link, [Port]},
                   permanent, 2000, worker, [hi_server]},
     Children = [HttpServer],
     RestartStrategy = {one_for_one, 4, 3600},
     {ok, {RestartStrategy, Children}}.
