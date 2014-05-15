-module(sc).

-compile(export_all).

%-define(ITER, 4).
-define(ITER, 128).

main([Host, Port0, N|BucketTypes]) when length(BucketTypes) > 0 ->
    Port = case Port0 of
               "rel" -> 8087;
               "dev" -> 10017;
               _ when is_list(Port0) -> list_to_integer(Port0)
           end,

    _ = [ run_sc_test(Host, Port,
                      list_to_integer(N),
                      list_to_binary(BucketType))
          || BucketType <- BucketTypes ];

main(_) ->
    io:format("usage: ERL_LIBS=path/to/riakc/deps escript sc.erl localhost rel <N> <testname>~n").


to_btb(<<"default">>) ->
    <<"sc_test">>;
to_btb(Other) ->
    {Other, <<"sc_test">>}.

run_sc_test(Host, Port, N, BucketType) ->
    BTB = to_btb(BucketType),
    Boss = self(),
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    %% Options = [{timeout, 6000}], %%build_options(Pid, BTB),
    ok = init_(Pid, BTB),
    
    io:format("~ntrying against ~p:~n", [BTB]),

    Start = erlang:now(),
    Processes = [spawn_link(fun() -> run(Boss, Host, Port, BTB, 0) end)
                 || _N <- lists:seq(1, N) ],
    Results = [receive
                    {done, Result} -> Result;
                    Error -> io:format("~p~n", [Error])
                end || _ <- Processes],
    End = erlang:now(),
    Duration = timer:now_diff(End, Start) / 1000000.0,
    Retry = lists:foldl(fun({_, C}, Acc0) -> Acc0 + C end, 0, Results),
    %% io:format("~p~n", [?LINE]),
    io:format("~n"),
    io:format("Duration: ~p ms, with total ~p retries (~p qps)~n",
              [Duration, Retry, (N*?ITER+Retry) / 1000.0 / Duration]),
    io:format("supposed to have ~p = (~p * ~p)~n", [N * ?ITER,
                                                    N, ?ITER]),

    check_(Pid, BTB, N*?ITER),
    ok = riakc_pb_socket:stop(Pid).

run(Boss, Host, Port, BTB, Options) ->
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    {ok, Counts} = run_(Pid, BTB, ?ITER, Options),
    riakc_pb_socket:stop(Pid),
    Boss ! {done, Counts}.

-define(KEY, <<"choobookaboot">>).

build_options(Pid, BTB) ->
    case riakc_pb_socket:get_bucket(Pid, BTB) of
        {ok, BucketProps} ->
            case lists:member({consistent, true}, BucketProps) of
                true ->
                    []; %%{if_none_match, true}];
                _ ->
                    []
            end;
        Error -> throw(Error)
    end.

init_(Pid, BTB) ->
    %% io:format("~p~n", [?LINE]),
    RiakObj = case riakc_pb_socket:get(Pid, BTB, ?KEY) of
                  {error, notfound} ->
                      riakc_obj:new(BTB, ?KEY, term_to_binary(0));
                  {ok, RiakObj0} ->
                      riakc_obj:update_value(RiakObj0, term_to_binary(0))
              end,
    ok = riakc_pb_socket:put(Pid, RiakObj).

check_(Pid, BTB, Expected) ->
    {ok, RiakObj} = riakc_pb_socket:get(Pid, BTB, ?KEY),
    Actual = binary_to_term(hd(riakc_obj:get_values(RiakObj))),
    io:format("actual value: ~p ", [Actual]),
    case Expected =:= Actual of
        true ->  io:format("\e[44m  ( ´ ▽ ` )ﾉ \e[0m~n");
        false -> io:format("\e[41m ヽ(`Д´)ﾉ︵ ┻━┻  \e[0m~n")
    end.


run_(_Pid, _BTB, 0, Retry) ->
    {ok, {0, Retry}};
run_(Pid, BTB, Count, Retry) ->
    io:format("\r ~p th task in ~p", [Count, self()]),

    {ok, RiakObj0} = riakc_pb_socket:get(Pid, BTB, ?KEY),
    C = binary_to_term(hd(riakc_obj:get_values(RiakObj0))),
    RiakObj = riakc_obj:update_value(RiakObj0, term_to_binary(C+1)),
    case riakc_pb_socket:put(Pid, RiakObj) of
        ok ->
            run_(Pid, BTB, Count-1, Retry);
        {error, <<"failed">>} ->
            %% io:format("~p>>~p~n", [E, ?LINE]),
            run_(Pid, BTB, Count, Retry+1)
    end.

