-module(ti_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

init([LSock]) ->
     ServerSup = {ti_server_sup, {ti_server_sup, start_link, [LSock]},
                   permanent, 2000, supervisor, [ti_server]},
     EventManager = {ti_event, {ti_event, start_link, []},
                     permanent, 2000, worker, [ti_event]},
     Children = [ServerSup, EventManager],
     RestartStrategy = {one_for_one, 4, 3600},
     {ok, {RestartStrategy, Children}}.

