-module(ti_event).

-export([start_link/0,
         add_handler/2,
         delete_handler/2,
         data_received/1,
         data_sent/1]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

data_received(Data) ->
    gen_event:notify(?SERVER, {data_received, Data}).

data_sent(Data) ->
    gen_event:notify(?SERVER, {data_sent, Data}).
