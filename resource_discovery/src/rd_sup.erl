-module(rd_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
     ResourceDiscoveryServer = {rd_event, {rd_event, start_link, []},
                                permanent, 2000, worker, [rd_event]},
     Children = [ResourceDiscoveryServer],
     RestartStrategy = {one_for_one, 4, 3600},
     {ok, {RestartStrategy, Children}}.

