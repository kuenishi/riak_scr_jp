# Map Reduce

riak-2.0.0pre20

## Map Reduceの概要

Riakクラスタ内の各ノードへ分散されたデータ、プロセスを用いて並列処理をおこなう仕組み。riak_pipeを用いて実現されている。

クエリサンプル

```
% curl -i -X PUT http://riak.example.com:8098/mapred -d @mapred.js
```

mapred.js

```javascript
{
  "inputs":
    "bucketname"
  ,
  "query":[
    {
      "map":{
         // ....
      }
    },
    {
      "reduce":{
          // ....
      }
    }
  ]
}
```

- input: データの入力ソース(inputs)にはをbucket名、key一覧、2i、検索クエリなどが指定可能。
- query: map, reduceフェーズをerlangモジュール、javascriptで記述可能

## riak_pipeの復習

- riak_pipe(Riak Pipelines)は"UNIX pipes for Riak"
- １つ以上のfittingによってpipelineが形成される
- inputは各々適切なvnodeへ配布されpipeline(fitting)の処理がなされる
- pipelineは最終的にsinkへそのoutputを送信し、結果を集約する

## どのように実現されているか

- クライアントから送信されたQueryを基にriak_pipeのpipelineを生成する。
- 入力ソースからBucket-Key一覧を生成し、これをriak_pipeのinputとして入力

## コード

### 概要

ざっくりと以下に分かれる

- APIレイヤ(HTTP, PB)
- riak_pipeとの中間レイヤ
- riak_pipeのpipelineレイヤ(map, reduceを始めとするfitting)
- vnodeレイヤ(inputsの生成)

### コード一覧

- riak_kv
  - API
    - riak_kv_pb_mapred.erl  PB API
    - riak_kv_wm_mapred.erl  HTTP API
  - pipe compat
    - riak_kv_mrc_map.erl         A pipe fitting that applies a function to a Riak object, and sends its results downstream.
    - riak_kv_mrc_pipe.erl        Riak KV MapReduce / Riak Pipe Compatibility
    - riak_kv_mrc_sink.erl        This FSM acts as a Riak Pipe sink
    - riak_kv_mrc_sink_sup.erl
  - convinience functions
    - riak_kv_mapreduce.erl        Map Reduce functions.
    - riak_kv_mapred_filters.erl   key filter builder & implementation.
    - riak_kv_w_reduce.erl         pre-reduce
    - riak_kv_mapred_json.erl      request parser (json)
    - riak_kv_mapred_term.erl      request parser (erlang term)
  - riak_pipe_vnode_worker
    - riak_kv_pipe_get.erl
    - riak_kv_pipe_index.erl
    - riak_kv_pipe_listkeys.erl
  - JS VM
    - riak_kv_js_manager.erl
    - riak_kv_js_sup.erl
    - riak_kv_js_vm.erl
- riak_pipe
- erlang_js

## Source Code Reading

### HTTP API

riak_kv_wm_mapred

```erlang
pipe_mapred(RD,
            #state{inputs=Inputs,
                   mrquery=Query,
                   timeout=Timeout}=State) ->
    case riak_kv_mrc_pipe:mapred_stream_sink(Inputs, Query, Timeout) of
        {ok, Mrc} ->
            case wrq:get_qs_value("chunked", "false", RD) of
                "true" ->
                    pipe_mapred_chunked(RD, State, Mrc);
                _ ->
                    pipe_mapred_nonchunked(RD, State, Mrc)
            end;
        {error, {Fitting, Reason}} ->
            {{halt, 400},
             send_error({error, [{phase, Fitting},
                                 {error, iolist_to_binary(Reason)}]}, RD),
             State}
    end.
```

### pipelineの形成

riak_kv_mrc_pipe

```
%% @doc Setup the MapReduce plumbing, including separate process to
%% receive output (the sink) and send input (the async sender), and a
%% delayed `pipe_timeout' message. This call returns a context record
%% containing details for each piece. Monitors are setup in the
%% process that calls this function, watching the sink and sender.
%%
%% See {@link receive_sink/1} for details about how to use this
%% context.
-spec mapred_stream_sink(input(), [query_part()], timeout()) ->
         {ok, #mrc_ctx{}} | {error, term()}.
mapred_stream_sink(Inputs, Query, Timeout) ->
    {ok, Sink} = riak_kv_mrc_sink:start(self(), []),        %% <-- riak_kv_mrc_sinkを起動
    Options = [{sink, #fitting{pid=Sink}},
               {sink_type, {fsm, sink_sync_period(), infinity}}],
    try mapred_stream(Query, Options) of                    %% <-- pipelineの設定
        {{ok, Pipe}, NumKeeps} ->
            %% catch just in case the pipe or sink has already died
            %% for any reason - we'll get a DOWN from the monitor later
            catch riak_kv_mrc_sink:use_pipe(Sink, Pipe),
            SinkMon = erlang:monitor(process, Sink),
            PipeRef = (Pipe#pipe.sink)#fitting.ref,
            Timer = erlang:send_after(Timeout, self(),     %% <-- timeout
                                      {pipe_timeout, PipeRef}),
            {Sender, SenderMon} =
                riak_kv_mrc_pipe:send_inputs_async(Pipe, Inputs),    %% <-- inputsの送信開始
            {ok, #mrc_ctx{ref=PipeRef,
                          pipe=Pipe,
                          sink={Sink,SinkMon},
                          sender={Sender,SenderMon},
                          timer={Timer,PipeRef},
                          keeps=NumKeeps}}
    catch throw:{badarg, Fitting, Reason} ->
            riak_kv_mrc_sink:stop(Sink),
            {error, {Fitting, Reason}}
    end.
```

```erlang
%% @doc Setup the MapReduce plumbing, preparted to receive inputs.
%% The caller should then use {@link send_inputs/2} or {@link
%% send_inputs/3} to give the query inputs to process.
%%
%% The second element of the return tuple is the number of phases that
%% requested to keep their inputs, and will need to be passed to
%% {@link collect_outputs/3} or {@link group_outputs/2} to get labels
%% compatible with HTTP and PB interface results.
-spec mapred_stream([query_part()], list()) ->
         {{ok, riak_pipe:pipe()}, NumKeeps :: integer()}.
mapred_stream(Query, Options) when is_list(Options) ->
    NumKeeps = count_keeps_in_query(Query),
    {riak_pipe:exec(mr2pipe_phases(Query),                       %% <-- fitting_spec、pipelineの設定
                    [{log, sink},{trace,[error]}]++Options),
     NumKeeps}.
```

```
%% @doc Convert a MapReduce query into a list of Pipe fitting specs.
-spec mr2pipe_phases([query_part()]) -> [ riak_pipe:fitting_spec() ].
mr2pipe_phases([]) ->
    [#fitting_spec{name=0,
                   module=riak_pipe_w_pass,
                   chashfun=follow}];
mr2pipe_phases(Query) ->
    ConstantHash = random_constant_hash(),      %% <-- ランダムにreduce用vnodeを決定

    %% first convert phase
    QueryT = list_to_tuple(Query),
    Numbered = lists:zip(Query, lists:seq(0, length(Query)-1)),
    Fittings0 = lists:flatten([mr2pipe_phase(P,I,ConstantHash,QueryT) ||
                                  {P,I} <- Numbered]),

    %% clean up naive 'keep' translationg
    Fs = fix_final_fitting(Fittings0),
    case lists:last(Query) of
        {_, _, _, false} ->
            %% The default action is to send results down to the next
            %% fitting in the pipe.  However, the last MapReduce query
            %% doesn't want those results.  So, add a "black hole"
            %% fitting that will stop all work items from getting to
            %% the sink and thus polluting our expected results.
            Fs ++ [#fitting_spec{name=black_hole,
                                 module=riak_pipe_w_pass,
                                 arg=black_hole,
                                 chashfun=follow}];
        _ ->
            Fs
    end.
```

```erlang
-spec mr2pipe_phase(query_part(),
                    Index :: integer(),
                    ConstantHash :: chash:index(),
                    Query :: tuple()) ->
         [ riak_pipe:fitting_spec() ].
mr2pipe_phase({map,FunSpec,Arg,Keep}, I, _ConstHash, QueryT) ->
    map2pipe(FunSpec, Arg, Keep, I, QueryT);
mr2pipe_phase({reduce,FunSpec,Arg,Keep}, I, ConstHash, _QueryT) ->
    reduce2pipe(FunSpec, Arg, Keep, I, ConstHash);
mr2pipe_phase({link,Bucket,Tag,Keep}, I, _ConstHash, QueryT)->
    link2pipe(Bucket, Tag, Keep, I, QueryT).
```

```erlang
-spec map2pipe(map_query_fun(), term(), boolean(),
               Index :: integer(), Query :: tuple()) ->
         [ riak_pipe:fitting_spec() ].
map2pipe(FunSpec, Arg, Keep, I, QueryT) ->
    PrereduceP = I+2 =< size(QueryT) andalso    %% <-- pre-reduce判定
        query_type(I+2, QueryT) == reduce andalso
        want_prereduce_p(I+1, QueryT),
    SafeArg = case FunSpec of
                  {JS, _} when (JS == jsfun orelse JS == jsanon),
                               is_list(Arg) ->
                      %% mochijson cannot encode these properties,
                      %% so remove them from the argument list
                      lists:filter(
                        fun(do_prereduce)     -> false;
                           ({do_prereduce,_}) -> false;
                           (_)                -> true
                        end,
                        Arg);
                  _ ->
                      Arg
              end,
    [#fitting_spec{name={kvget_map,I},                       %% BKeyからRObjectをGet
                   module=riak_kv_pipe_get,
                   chashfun={riak_kv_pipe_get, bkey_chash},
                   nval={riak_kv_pipe_get, bkey_nval}},
     #fitting_spec{name={xform_map,I},                       %% 指定したMap functionの実行
                   module=riak_kv_mrc_map,
                   arg={FunSpec, SafeArg},
                   chashfun=follow}]
     ++
     [#fitting_spec{name=I,                                  %% 他vnodeへ転送
                    module=riak_pipe_w_tee,
                    arg=sink,
                    chashfun=follow} || Keep]
     ++
     if PrereduceP ->
             {reduce, R_FunSpec, R_Arg, _Keep} = element(I+2, QueryT),
             [#fitting_spec{name={prereduce,I},                         %% Pre-Reduce
                            module=riak_kv_w_reduce,
                            arg={rct,
                                 riak_kv_w_reduce:reduce_compat(R_FunSpec),
                                 R_Arg},
                            chashfun=follow}];
        true ->
             []
     end.
```

### 入力ソース

riak_kv_mrc_pipe

```erlang
-spec send_inputs_async(riak_pipe:pipe(), input(), timeout()) ->
         {Sender::pid(), MonitorRef::reference()}.
send_inputs_async(Pipe, Inputs, Timeout) ->
    spawn_monitor(
      fun() ->
              erlang:link(Pipe#pipe.builder),
              case send_inputs(Pipe, Inputs, Timeout) of
                  ok ->
                      %% monitoring process sees a 'normal' exit
                      %% (and linked builder is left alone)
                      ok;
                  Error ->
                      %% monitoring process sees an 'error' exit
                      %% (and linked builder dies)
                      exit(Error)
              end
      end).
```

```erlang
-spec send_inputs(riak_pipe:pipe(), input(), timeout()) ->
         ok | term().
send_inputs(Pipe, BucketKeyList, _Timeout) when is_list(BucketKeyList) ->   %% Bucket-Keyのリストはそのままqueue_workへ流し込む
    try [ok = riak_pipe:queue_work(Pipe, BKey)
         || BKey <- BucketKeyList] of
        _ ->
            riak_pipe:eoi(Pipe),
            ok
    catch error:{badmatch,{error,_}=Error} ->
            Error
    end;
send_inputs(Pipe, Bucket, Timeout) when is_binary(Bucket) ->
    riak_kv_pipe_listkeys:queue_existing_pipe(Pipe, Bucket, Timeout);       %% Bucket内の全Keyを入力
send_inputs(Pipe, {Type, Bucket}, Timeout) when is_binary(Type), is_binary(Bucket) ->
    riak_kv_pipe_listkeys:queue_existing_pipe(Pipe, {Type, Bucket}, Timeout);
send_inputs(Pipe, {Bucket, FilterExprs}, Timeout) ->
    case riak_kv_mapred_filters:build_filter(FilterExprs) of
        {ok, Filters} ->
            riak_kv_pipe_listkeys:queue_existing_pipe(
              Pipe, {Bucket, Filters}, Timeout);
        Error ->
            Error
    end;
send_inputs(Pipe, {index, Bucket, Index, Key}, Timeout) ->                  %% 2iを入力
    Query = {eq, Index, Key},
    case riak_core_capability:get({riak_kv, mapred_2i_pipe}, false) of
        true ->
            riak_kv_pipe_index:queue_existing_pipe(
              Pipe, Bucket, Query, Timeout);
        _ ->
            %% must use modfun form if there are 1.0 nodes in the cluster,
            %% because they do not have the riak_kv_pipe_index module
            NewInput = {modfun, riak_index, mapred_index, [Bucket, Query]},
            send_inputs(Pipe, NewInput, Timeout)
    end;
%% search等は省略
```

riak_kv_pipe_listkeys

```erlang
queue_existing_pipe(Pipe, Input, Timeout) ->
    Bucket = case Input of
                 {T, B} when is_binary(T), is_binary(B) ->
                     %% type and bucket
                     {T, B};
                 B when is_binary(B) ->
                     %% just bucket
                     B;
                 {B, Filter} when is_list(Filter) ->
                     %% bucket with filter
                     B
             end,
    %% make our tiny pipe
    [{_Name, Head}|_] = Pipe#pipe.fittings,
    Period = riak_kv_mrc_pipe:sink_sync_period(),
    {ok, LKP} = riak_pipe:exec([#fitting_spec{name=listkeys,
                                              module=?MODULE,
                                              nval=1}],
                               [{sink, Head},
                                {trace, [error]},
                                {log, {sink, Pipe#pipe.sink}},
                                {sink_type, {fsm, Period, infinity}}]),

    %% setup the cover operation
    ReqId = erlang:phash2({self(), os:timestamp()}), %% stolen from riak_client
    BucketProps = riak_core_bucket:get_bucket(Bucket),
    NVal = proplists:get_value(n_val, BucketProps),
    {ok, Sender} = riak_pipe_qcover_sup:start_qcover_fsm(
                     [{raw, ReqId, self()},
                      [LKP, Input, NVal]]),

    %% wait for cover to hit everything
    %% 略
```

```erlang
-spec process(term(), boolean(), state()) -> {ok | {error, term()}, state()}.
process(Input, _Last, #state{p=Partition, fd=FittingDetails}=State) ->
    case Input of
        {cover, FilterVNodes, {T, B}} when is_binary(T), is_binary(B) ->
            %% bucket and type
            Bucket = {T, B},
            Filters = [];
        {cover, FilterVNodes, {Bucket, Filters}} ->
            ok;
        {cover, FilterVNodes, Bucket} ->
            Filters = [];
        {T, B} when is_binary(T), is_binary(B) ->
            %% bucket and type
            Bucket = {T, B},
            Filters = [],
            FilterVNodes = [];
        {Bucket, Filters} ->
            FilterVNodes = [];
        Bucket ->
            Filters = [],
            FilterVNodes = []
    end,
    ReqId = erlang:phash2({self(), os:timestamp()}), % stolen from riak_client
    riak_core_vnode_master:coverage(
      riak_kv_keys_fsm:req(Bucket, Filters),
      {Partition, node()},
      FilterVNodes,
      {raw, ReqId, self()},
      riak_kv_vnode_master),
    Result = keysend_loop(ReqId, Partition, FittingDetails),
    {Result, State}.
```

### sink

```erlang
pipe_mapred_nonchunked(RD, State, Mrc) ->
    case riak_kv_mrc_pipe:collect_sink(Mrc) of
        {ok, Results} ->
            JSONResults =
                case Mrc#mrc_ctx.keeps < 2 of
                    true ->
                        [riak_kv_mapred_json:jsonify_not_found(R)
                         || R <- Results];
                    false ->
                        [[riak_kv_mapred_json:jsonify_not_found(PR)
                          || PR <- PhaseResults]
                         || PhaseResults <- Results]
                end,
            HasMRQuery = State#state.mrquery /= [],
            JSONResults1 = riak_kv_mapred_json:jsonify_bkeys(JSONResults, HasMRQuery),
            riak_kv_mrc_pipe:cleanup_sink(Mrc),
            {true,
             wrq:set_resp_body(mochijson2:encode(JSONResults1), RD),
             State};
        {error, {sender_died, Error}} ->
            %% the sender links to the builder, so the builder has
            %% already been torn down
            riak_kv_mrc_pipe:cleanup_sink(Mrc),
            {{halt, 500}, send_error(Error, RD), State};
        {error, {sink_died, Error}} ->
            %% pipe monitors the sink, so the sink death has already
            %% detroyed the pipe
            riak_kv_mrc_pipe:cleanup_sink(Mrc),
            {{halt, 500}, send_error(Error, RD), State};
        {error, timeout} ->
            riak_kv_mrc_pipe:destroy_sink(Mrc),
            {{halt, 500}, send_error({error, timeout}, RD), State};
        {error, {From, Info}} ->
            riak_kv_mrc_pipe:destroy_sink(Mrc),
            Json = riak_kv_mapred_json:jsonify_pipe_error(From, Info),
            {{halt, 500}, send_error({error, Json}, RD), State}
    end.
```


### fitting_spec

```erlang
#fitting_spec {
   name = foo, %% term(), anything to help you help you find results
               %% and log output from this fitting

   module = riak_pipe_w_pass, %% atom(), must be a valid module name,
                              %% the name of the module implementing the
                              %% riak_pipe_vnode_worker behavior for this
                              %% fitting

   arg = undefined, %% term(), opaque static argument included in the
                    %% fitting_details structure passed to the worker
                    %% module (useful for initial configuration)

   chashfun = fun(_) -> <<0:160/integer>> end,
                              %% arity-1 function() | 'follow'
                              %% specification of how to distribute
                              %% inputs for this fitting

   nval = 1,%% positive integer, default 1, indicates how many vnodes
            %% may be asked to handle an input for this fitting,
            %% before declaring the input unfit for processing

   q_limit = 64 %% positive integer, default 64, sets the maximum
                %% number of elements allowed in a worker's queue,
                %% the actual queue limit is the lesser of this value
                %% and the worker_q_limit variable in riak_pipe's
                %% application environment (default 4096)
}```
