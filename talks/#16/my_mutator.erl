-module(my_mutator).

-behaviour(riak_kv_mutator).
-export([mutate_put/5, mutate_get/1]).

-spec mutate_get(InObject :: riak_object:riak_object()) -> riak_object:riak_object() | 'notfound'.
mutate_get(RObject) ->
    io:format("RObject: ~p~n", [RObject]),
    RObject.

-spec mutate_put(Meta :: dict(), Value :: term(), ExposedMeta :: dict(),
    FullObject :: riak_object:riak_object(), Props :: orddict:orddict()) ->
        {dict(), term(), dict()}.
mutate_put(Meta, Value, ExposedMeta, FullObject, Props) ->
    io:format("Meta:        ~p~n", [Meta]),
    io:format("Value:       ~p~n", [Value]),
    io:format("ExposedMeta: ~p~n", [ExposedMeta]),
    io:format("FullObject:  ~p~n", [FullObject]),
    io:format("Props:       ~p~n", [Props]),

    RMeta = case dict:find(<<"X-Riak-Meta">>, Meta) of
        error  -> [];
        {ok, M} -> M
    end,
    NewRMeta = RMeta ++ [{"X-Riak-Meta-Dummy", "dummy"}],
    io:format("NewRMeta:        ~p~n", [NewRMeta]),

    case riak_object:key(FullObject) of
        <<"alice">> ->
            {Meta, Value, dict:store(<<"X-Riak-Meta">>, NewRMeta, ExposedMeta)};
        <<"bob">> ->
            {dict:store(<<"X-Riak-Meta">>, NewRMeta, Meta), <<"dummy">>, ExposedMeta};
        _ ->
            {Meta, Value, ExposedMeta}
    end.
