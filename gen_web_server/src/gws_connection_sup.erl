-module(gws_connection_sup).

-behaviour(supervisor).

-export([start_link/4, start_child/1]).

-export([init/1]).

start_link(Callbacks, Ip, Port, UserArgs) ->
    {ok, Pid} = supervisor:start_link(?MODULE, [Callbacks, Ip,
                                                Port, UserArgs]),
    start_child(Pid),
    {ok, Pid}.

start_child(Server) ->
    supervisor:start_child(Server, []).

init([Callbacks, Ip, Port, UserArgs]) ->
    BasicSockOpts = [binary,
                     {active, false},
                     {packet, http_bin},
                     {reuseaddr, true}],
    SockOpts = case Ip of
                   undefined -> BasicSockOpts;
                   _ -> [{ip, Ip} | BasicSockOpts]
               end,
    {ok, LSock} = gen_tcp:listen(Port, SockOpts),
    Server = {gws_server, {gws_server, start_link,
                           [Callbacks, LSock, UserArgs]},
              temporary, brutal_kill, worker, [gws_server]},
    RestartStrategy = {simple_one_for_one, 1000, 3600},
    {ok, {RestartStrategy, [Server]}}.
