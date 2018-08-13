-module(rd_server).

-behaviour(gen_server).

-export([start_link/0,
         state/0,
         add_target_resource/1,
         add_local_resource/1,
         fetch_resource_locations/1,
         trade_resources/0]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target_resources = [],
                local_resources = [],
                found_resources = #{}}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

state() ->
    gen_server:call(?SERVER, state).

add_target_resource(Resource) ->
    gen_server:cast(?SERVER, {add_target_resource, Resource}).

add_local_resource(Resource) ->
    gen_server:cast(?SERVER, {add_local_resource, Resource}).

fetch_resource_locations(Resource) ->
    gen_server:call(?SERVER, {fetch_resource_locations, Resource}).

trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

handle_call({fetch_resource_locations, Resource}, _From, State) ->
    {reply, maps:find(Resource, State#state.found_resources), State};
handle_call(state, _From, State) ->
    {reply, State, State}.

handle_cast({add_target_resource, Resource}, State) ->
    TargetResources = State#state.target_resources,
    NewTargetResources = [Resource | lists:delete(Resource, TargetResources)],
    {noreply, State#state{target_resources = NewTargetResources}};
handle_cast({add_local_resource, Resource}, State) ->
    LocalResources = State#state.local_resources,
    NewLocalResources = [Resource | lists:delete(Resource, LocalResources)],
    {noreply, State#state{local_resources = NewLocalResources}};
handle_cast(trade_resources, State) ->
    LocalResources = State#state.local_resources,
    AllReachableNodes = [node() | nodes()],
    lists:foreach(
      fun(Node) ->
              gen_server:cast({?SERVER, Node},
                              {trade_resources, {node(), node(), LocalResources}})
      end,
      AllReachableNodes),
    {noreply, State};
handle_cast({trade_resources, {ReplyTo, ResourceLocation, ReceivedResources}},
            #state{local_resources = LocalResources,
                   found_resources = FoundResources,
                   target_resources = TargetResources} = State) ->
    ReceivedTargetResources = get_target_resources(TargetResources,
                                                   ReceivedResources,
                                                   ResourceLocation),
    NewFoundResources = add_resources(ReceivedTargetResources, FoundResources),
    case ReplyTo of
        noreply -> ok;
        _ -> gen_server:cast({?SERVER, ReplyTo},
                             {trade_resources, {noreply, node(), LocalResources}})
    end,
    {noreply, State#state{found_resources = NewFoundResources}}.

get_target_resources(TargetResources, ReceivedResources, ResourceLocation) ->
    Fun = fun(Resource, Acc) ->
                  case lists:member(Resource, ReceivedResources) of
                      true ->
                          [{Resource, ResourceLocation} | Acc];
                      error -> Acc
                  end
          end,
    lists:foldl(Fun, [], TargetResources).

add_resources([{Resource, ResourceLocation}|Rest], Final) ->
    add_resources(Rest, add_resource(Resource, ResourceLocation, Final));
add_resources([], Final) -> Final.

add_resource(Resource, Location, Resources) ->
    case maps:find(Resource, Resources) of
        {ok, Locations} ->
            NewList = [Location | lists:delete(Location, Locations)],
            maps:put(Resource, NewList, Resources);
        error ->
            maps:put(Resource, [Location], Resources)
    end.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_Old, State, _New) ->
    {ok, State}.
