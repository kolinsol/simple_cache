-module(rd_server).

-behaviour(gen_server).

-export([start_link/0,
         state/0,
         add_target_resource_type/1,
         add_local_resource/2,
         fetch_resources/1,
         trade_resources/0]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target_resource_types = [],
                local_resources = #{},
                found_resources = #{}}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

state() ->
    gen_server:call(?SERVER, state).

add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_local_resource(Type, Instance) ->
    gen_server:cast(?SERVER, {add_local_resource, Type, Instance}).

fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

handle_call({fetch_resources, Type}, _From, State) ->
    {reply, maps:find(Type, State#state.found_resources), State};
handle_call(state, _From, State) ->
    {reply, State, State}.

handle_cast({add_target_resource_type, Type}, State) ->
    TargetTypes = State#state.target_resource_types,
    NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
    {noreply, State#state{target_resource_types = NewTargetTypes}};
handle_cast({add_local_resource, Type, Instance}, State) ->
    LocalResources = State#state.local_resources,
    NewLocalResources = add_resource(Type, Instance, LocalResources),
    {noreply, State#state{local_resources = NewLocalResources}};
handle_cast(trade_resources, State) ->
    LocalResources = State#state.local_resources,
    AllNodes = [node() | nodes()],
    lists:foreach(
      fun(Node) ->
              gen_server:cast({?SERVER, Node},
                              {trade_resources, {node(), node(), LocalResources}})
      end,
      AllNodes),
    {noreply, State};
handle_cast({trade_resources, {ReplyTo, Location, RemoteResources}},
            #state{local_resources = LocalResources,
                   found_resources = FoundResources,
                   target_resource_types = TargetTypes} = State) ->
    FilteredRemotes = resources_for_types(TargetTypes, RemoteResources, Location),
    NewFoundResources = add_resources(FilteredRemotes, FoundResources),
    case ReplyTo of
        noreply -> ok;
        _ -> gen_server:cast({?SERVER, ReplyTo},
                             {trade_resources, {noreply, node(), LocalResources}})
    end,
    {noreply, State#state{found_resources = NewFoundResources}}.

resources_for_types(Types, Resources, Location) ->
    Fun = fun(Type, Acc) ->
                  case maps:find(Type, Resources) of
                      {ok, List} ->
                          [{Type, {Instance, Location}} || Instance <- List] ++ Acc;
                      error -> Acc
                  end
          end,
    lists:foldl(Fun, [], Types).

add_resources([{Type, {_Instance, _Location} = Resource}|Rest], Final) ->
    add_resources(Rest, add_resource(Type, Resource, Final));
add_resources([], Final) -> Final.

add_resource(Type, Instance, Resources) ->
    case maps:find(Type, Resources) of
        {ok, Instnces} ->
            NewList = [Instance | lists:delete(Instance, Instnces)],
            maps:put(Type, NewList, Resources);
        error ->
            maps:put(Type, [Instance], Resources)
    end.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_Old, State, _New) ->
    {ok, State}.
