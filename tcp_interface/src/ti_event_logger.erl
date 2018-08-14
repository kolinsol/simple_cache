-module(ti_event_logger).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {}).

add_handler() ->
    ti_event:add_handler(?MODULE, []).

delete_handler() ->
    ti_event:delete_handler(?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_call(_Req, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

handle_event({data_received, Data}, State) ->
    error_logger:info_msg("DATA RECEIVED: ~w~n", [Data]),
    {ok, State};
handle_event({data_sent, Data}, State) ->
    error_logger:info_msg("DATA SENT: ~w~n", [Data]),
    {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _New) ->
    {ok, State}.

