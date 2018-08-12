-module(sc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = ensure_contact(),
    case sc_sup:start_link() of
        {ok, Pid} ->
            sc_store:init(),
            sc_event_logger:add_handler(),
            {ok, Pid};
        Other -> {error, Other}
    end.

ensure_contact() ->
    DefaultContactNodes = ['contact1@localhost', 'contact2@localhost'],
    case get_env(simple_cache, contact_nodes, DefaultContactNodes) of
        [] -> {error, no_contact_nodes};
        ContactNodes -> ensure_contact(ContactNodes)
    end.

ensure_contact(ContactNodes) ->
    AvailableNodes = [Node || Node <- ContactNodes, net_adm:ping(Node) =:= pong],
    case AvailableNodes of
        [] -> {error, no_contact_nodes_available};
        _ ->
            DefaultWaitTime = 6000,
            WaitTime = get_env(simple_cache, wait_time, DefaultWaitTime),
            wait_for_nodes(length(AvailableNodes), WaitTime)
    end.

wait_for_nodes(NodeNumber, WaitTime) ->
    Iterations = 10,
    IterationTime = round(WaitTime/Iterations),
    wait_for_nodes(NodeNumber, IterationTime, Iterations).

wait_for_nodes(_NodeNumber, _IterationTime, 0) ->
    ok;
wait_for_nodes(NodeNumber, IterationTime, Iterations) ->
    case length(nodes()) > NodeNumber of
        true -> ok;
        false ->
            timer:sleep(IterationTime),
            wait_for_nodes(NodeNumber, IterationTime, Iterations - 1)
    end.

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
        undefined -> Default;
        {ok, Value} -> Value
    end.

stop(_State) -> ok.
