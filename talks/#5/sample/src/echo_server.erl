-module(echo_server).

-compile([export_all]).
-record(state,{conter=0}).

start() ->
    Pid  = spawn_link(fun() -> loop(init()) end),
    register(?MODULE, Pid).

init() ->
    {#state{}}.

loop(State) ->
    receive
        {From, Msg} -> From ! handle(Msg);
        _Other       -> erlang:error("invalid message format.")
    end,
    loop(State).

-spec(handle(term()) -> {pid(), term()}).
handle(Msg)  ->
    {self(), Msg}.

echo(Request) ->
    ?MODULE ! {self(), Request},
    receive
        {_From, Response} -> io:format("received ~p~n", [Response])
    end.
