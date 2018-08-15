-module(ti_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {lsock}).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    ti_event:data_received(RawData),
    NewState = handle_data(Socket, RawData, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    ti_server_sup:start_child(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_data(Socket, RawData, State) ->
    try
        {Function, RawArgList} =
            lists:splitwith(fun(C) -> C =/= $[ end, RawData),
        {ok, Toks, _Line} = erl_scan:string(RawArgList ++ "."),
        {ok, Args} = erl_parse:parse_term(Toks),
        Result = apply(simple_cache, list_to_atom(Function), Args),
        ti_event:data_sent(Result),
        gen_tcp:send(Socket, io_lib:fwrite("OK:~p.~n", [Result]))
    catch
        _Class:Err ->
            ti_event:data_sent(Err),
            gen_tcp:send(Socket, io_lib:fwrite("ERROR:~p.~n", [Err]))
    end,
    State.
