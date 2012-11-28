% Riak Source Code Reading #1
% csakatoku
% 2012-11-28

## おまえだれよ

* Zynga Japan
* 緋村剣心に対する志々雄真実の役割の仕事

## 今日のゴール

- Riakのbitcask, eleveldbバックエンドのコードをストイックに読んで行きましょう
- 必要な時にはRiakのドキュメントを読みましょう

## なぜこのテーマを選んだか？

- The RingとかGossipingとかEventual ConsistencyとかVector-Clockとか正しく説明できる自信がないし・・・
- PluggableなErlangのコードを書くパターンを見てみたかったし・・・
- 簡単そうだったら自分でBackendを書いてみようかなと思ったり・・・

## 今日やらないこと

- バックエンドのチューニングまでは踏み込みません
- bitcask, eleveldbの実装までは踏み込みません
- バックエンドよりはそのひとつ上のvnodeのコードの方が面白いのですが踏み込みません
- fold_*の関数が実際のところどのように使われているかよく分かっていません

## Backendのドキュメント
公式のドキュメントにチューニングの指針や実装の概要まで書いてあります。

http://docs.basho.com/riak/latest/tutorials/System-Planning/#Backend

## Backend: Memory
テスト用途や、あるいは揮発性の少量のデータを保存しておくためのバックエンド。

http://docs.basho.com/riak/latest/tutorials/choosing-a-backend/Memory/

## Backend: Multi
1つのRiakインスタンス上で複数のバックエンドを使うためのバックエンド。

http://docs.basho.com/riak/latest/tutorials/choosing-a-backend/Multi/

## Backend: HanoiDB
LevelDBのPure Erlangでの再実装。

- https://speakerdeck.com/krestenkrab/hanoidb-and-other-riak-hacks
- https://github.com/krestenkrab/hanoidb
- https://github.com/basho-labs/riak_kv_hanoidb_backend

## 原則: Bitcask vs LevelDB

http://docs.basho.com/riak/latest/tutorials/choosing-a-backend/

- Keyの数が比較的少なく、かつ最大限のスループットが要求されるならばBitcask
- Keyの数が非常に多いならば、LevelDB

## 実例: Bitcask vs LevelDB
デンマークのCommon Medicine Cardシステム。

```
| Person Data  | Audit Log    |
|:------------:| :-----------:|
| ~5M entities | ~5B entries  |
| read/write   | mostly write |
| Bitcask      | LevelDB      |
```

https://speakerdeck.com/krestenkrab/hanoidb-and-other-riak-hacks からの引用。

## Key-Value Store
そもそもRiakの場合、Keyって何で、Valueってなんだろう？

- Key: {Bucket, Key}
- Value: Object(riak_object)

## 概念: Keys
http://docs.basho.com/riak/latest/references/appendices/concepts/Keys-and-Objects/#Keys

- Keys in Riak are simply binary values (or strings) used to identify Objects.
- Each bucket appears to represent a separate keyspace.

## 実装: Keys

Bitcask

```
    BitcaskKey = term_to_binary({Bucket, Key}),
```

eleveldb

```
to_object_key(Bucket, Key) ->
    sext:encode({o, Bucket, Key}).

to_index_key(Bucket, Key, Field, Term) ->
    sext:encode({i, Bucket, Field, Term, Key}).
```

## 概念: Objects
http://docs.basho.com/riak/latest/references/appendices/concepts/Keys-and-Objects/#Objects

- Riak Objects are essentially structs identified by bucket and key and composed of the following parts: a bucket, key, vector clock, and a list of metadata-value pairs.

## 実装: Objects
riak_kv/src/riak_object.erl

```
%% Opaque container for Riak objects, a.k.a. riak_object()
-record(r_object, {
          bucket :: bucket(),
          key :: key(),
          contents :: [#r_content{}],
          vclock = vclock:fresh() :: vclock:vclock(),
          updatemetadata=dict:store(clean, true, dict:new()) :: dict(),
          updatevalue :: term()
         }).
```

## 例: Objects

https://github.com/basho/riak/blob/master/README.org#connecting-a-client-to-riak

```
   (riaktest@127.0.0.1)6> O0 = riak_object:new(<<"groceries">>, <<"mine">>, ["bread"]).
   O0 = riak_object:new(<<"groceries">>, <<"mine">>, ["bread"]).
   {r_object,<<"groceries">>,<<"mine">>,
          [{r_content,{dict,0,16,16,8,80,48,
                            {[],[],[],[],[],[],[],[],[],[],[],[],[],[],...},
                            {{[],[],[],[],[],[],[],[],[],[],[],[],...}}},
                      ["bread"]}],
          [],
          {dict,1,16,16,8,80,48,
                {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],...},
                {{[],[],[],[],[],[],[],[],[],[],[],[],[],...}}},
          undefined}

    (riaktest@127.0.0.1)3> C:put(O0, 1).
```

## バックエンドモジュール

```
% ls deps/riak_kv/src/*backend*.erl
deps/riak_kv/src/riak_kv_backend.erl
deps/riak_kv/src/riak_kv_eleveldb_backend.erl
deps/riak_kv/src/riak_kv_multi_backend.erl
deps/riak_kv/src/riak_kv_bitcask_backend.erl
deps/riak_kv/src/riak_kv_memory_backend.erl
deps/riak_kv/src/riak_kv_yessir_backend.erl
```

riak_kv_backendがbehaviorで、riak_kv_*_backend.erlが実際のバックエンド実装。

```
-behavior(riak_kv_backend).
```

## バックエンドAPI

```
%% KV Backend API
-export([api_version/0,
         capabilities/1,
         capabilities/2,
         start/2,
         stop/1,
         get/3,
         put/5,
         delete/4,
         drop/1,
         fold_buckets/4,
         fold_keys/4,
         fold_objects/4,
         is_empty/1,
         status/1,
         callback/3]).
```

## capabilities

```
% grep CAPABILITIES deps/riak_kv/src/*.erl | grep define
deps/riak_kv/src/riak_kv_bitcask_backend.erl:-define(CAPABILITIES, [async_fold]).
deps/riak_kv/src/riak_kv_eleveldb_backend.erl:-define(CAPABILITIES, [async_fold, indexes]).
deps/riak_kv/src/riak_kv_memory_backend.erl:-define(CAPABILITIES, [async_fold, indexes]).
deps/riak_kv/src/riak_kv_multi_backend.erl:-define(CAPABILITIES, [async_fold]).
deps/riak_kv/src/riak_kv_yessir_backend.erl:-define(CAPABILITIES, [async_fold]).
```

実際にcapabilitiesを呼び出しているコードは主に`riak_kv/src/riak_kv_vnode.erl`

## Backend: Bitcask
http://docs.basho.com/riak/latest/tutorials/choosing-a-backend/Bitcask/

- merge
- sync

## Bitcask Sync
データをディスクに書き出す戦略。

- `none`: OSのsyncにまかせる
- `o_sync`: すべての書き込みにO_SYNCフラグを使う
- `{seconds, N}`: N秒ごとに書き出しを強制する

## Bitcask Merge
不要なデータを削除する仕組み。

```
{bitcask, [
        ...,
        {merge_window, always}, %% Span of hours during which merge is acceptable.

        %% Trigger a merge if any of the following are true:
        {frag_merge_trigger, 60}, %% fragmentation >= 60%
        {dead_bytes_merge_trigger, 536870912}, %% dead bytes > 512 MB

        %% Conditions that determine if a file will be examined during a merge:
        {frag_threshold, 40}, %% fragmentation >= 40%
        {dead_bytes_threshold, 134217728}, %% dead bytes > 128 MB
        {small_file_threshold, 10485760}, %% file is < 10MB
        ...
]}
```

## riak_kv_bitcask_backend:start
バックエンドを起動しているのは、`riak_kv_vnode.erl`

```
init([Index]) ->
    Mod = app_helper:get_env(riak_kv, storage_backend),
    Configuration = app_helper:get_env(riak_kv),
    BucketBufSize = app_helper:get_env(riak_kv, bucket_buffer_size, 1000),
    IndexBufSize = app_helper:get_env(riak_kv, index_buffer_size, 100),
    KeyBufSize = app_helper:get_env(riak_kv, key_buffer_size, 100),
    WorkerPoolSize = app_helper:get_env(riak_kv, worker_pool_size, 10),
    {ok, VId} = get_vnodeid(Index),
    DeleteMode = app_helper:get_env(riak_kv, delete_mode, 3000),
    AsyncFolding = app_helper:get_env(riak_kv, async_folds, true) == true,
    case catch Mod:start(Index, Configuration) of
        {ok, ModState} ->
            %% Get the backend capabilities
```

## riak_kv_bitcask_backend:start
start時の主な処理は以下の部分。bitcaskをオープンし、mergeとsyncをスケジューリングする。

```
                    case bitcask:open(filename:join(DataRoot, DataDir), BitcaskOpts) of
                        Ref when is_reference(Ref) ->
                            check_fcntl(),
                            schedule_merge(Ref),
                            maybe_schedule_sync(Ref),
                            {ok, #state{ref=Ref,
                                        data_dir=DataDir,
                                        root=DataRoot,
                                        opts=BitcaskOpts,
                                        partition=Partition}};

```

## riak_kv_bitcask_backend:callback
syncの処理。`bitcask:sync`を実行して、再度スケジューリング。

```
callback(Ref,
         {sync, SyncInterval},
         #state{ref=Ref}=State) when is_reference(Ref) ->
    bitcask:sync(Ref),
    schedule_sync(Ref, SyncInterval),
    {ok, State};
```

## riak_kv_bitcask_backend:callback
mergeの処理。必要ならばmergeを実行して、再度スケジューリング。

```
callback(Ref,
         merge_check,
         #state{ref=Ref,
                data_dir=DataDir,
                opts=BitcaskOpts,
                root=DataRoot}=State) when is_reference(Ref) ->
    case bitcask:needs_merge(Ref) of
        {true, Files} ->
            BitcaskRoot = filename:join(DataRoot, DataDir),
            bitcask_merge_worker:merge(BitcaskRoot, BitcaskOpts, Files);
        false ->
            ok
    end,
    schedule_merge(Ref),
    {ok, State};
```

## riak_kv_bitcask_backend:stop
stopは`bitcask:close`を呼び出しているだけ。

```
stop(#state{ref=Ref}) ->
    case Ref of
        undefined ->
            ok;
        _ ->
            bitcask:close(Ref)
    end.
```

## riak_kv_bitcask_backend:get
`bitcask:get`を呼び出しているだけで自明。

```
get(Bucket, Key, #state{ref=Ref}=State) ->
    BitcaskKey = term_to_binary({Bucket, Key}),
    case bitcask:get(Ref, BitcaskKey) of
        {ok, Value} ->
            {ok, Value, State};
        not_found  ->
            {error, not_found, State};
        {error, nofile}  ->
            {error, not_found, State};
        {error, Reason} ->
            {error, Reason, State}
    end.
```

## riak_kv_bitcask_backend:put
`bitcask:put`を呼び出しているだけで自明。

```
put(Bucket, PrimaryKey, _IndexSpecs, Val, #state{ref=Ref}=State) ->
    BitcaskKey = term_to_binary({Bucket, PrimaryKey}),
    case bitcask:put(Ref, BitcaskKey, Val) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.
```

## riak_kv_bitcask_backend:delete
`bitcask:delete`を呼び出しているだけで自明。

```
delete(Bucket, Key, _IndexSpecs, #state{ref=Ref}=State) ->
    BitcaskKey = term_to_binary({Bucket, Key}),
    case bitcask:delete(Ref, BitcaskKey) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.
```

## riak_kv_bitcask_backend:fold_*

`riak_kv_bitcask_backend:fold_buckets`, `riak_kv_bitcask_backend:fold_keys`, `riak_kv_bitcask_backend:fold_objects`の3つ。

- riak_kv_eleveldb_backend:fold_*の方が流れを追いやすそうなので省略。

## riak_kv_bitcask_backend:drop

かなり泥臭いことをやっているので、深追いする時間がなかったので省略。すみません、すみません。

```
%% @doc Delete all objects from this bitcask backend
%% @TODO once bitcask has a more friendly drop function
%%  of its own, use that instead.
-spec drop(state()) -> {ok, state()} | {error, term(), state()}.
```

## riak_kv_bitcask_backend:is_empty
要調査。すみません、すみません。

```
%% @doc Returns true if this bitcasks backend contains any
%% non-tombstone values; otherwise returns false.
-spec is_empty(state()) -> boolean().
    %% Estimate if we are empty or not as determining for certain
    %% requires a fold over the keyspace that may block. The estimate may
    %% return false when this bitcask is actually empty, but it will never
    %% return true when the bitcask has data.
    bitcask:is_empty_estimate(Ref).
```

## riak_kv_bitcask_backend:status
`bitcask:status`を呼び出す。自明。

```
%% @doc Get the status information for this bitcask backend
-spec status(state()) -> [{atom(), term()}].
status(#state{ref=Ref}) ->
    {KeyCount, Status} = bitcask:status(Ref),
    [{key_count, KeyCount}, {status, Status}].
```

## eleveldb

http://docs.basho.com/riak/latest/tutorials/choosing-a-backend/LevelDB/

## riak_kv_eleveldb_backend:start
実質的な仕事は`riak_kv_eleveldb_backend:open_db`がやっている。

```
start(Partition, Config) ->
    %% 省略
    case open_db(S0) of
        {ok, State} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.
```

## riak_kv_eleveldb_backend:open_db
`eleveldb:open`を行い、失敗した場合は`eleveldb_open_retries`の回数だけリトライする。

```
open_db(State) ->
    RetriesLeft = app_helper:get_env(riak_kv, eleveldb_open_retries, 30),
    open_db(State, max(1, RetriesLeft), undefined).

open_db(_State0, 0, LastError) ->
    {error, LastError};
open_db(State0, RetriesLeft, _) ->
    case eleveldb:open(State0#state.data_root, State0#state.open_opts) of
        {ok, Ref} ->
            {ok, State0#state { ref = Ref }};
        {error, {db_open, OpenErr}=Reason} ->
            case lists:prefix("IO error: lock ", OpenErr) of
                true ->
                    %% 省略
                    open_db(State0, RetriesLeft - 1, Reason);
                false ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

## riak_kv_eleveldb_backend:callback
Bitcaskバックエンドと違って定期的に実行する処理がないので、何もしていない。(もしくは、eleveldbモジュール側がそういった処理の面倒をみてくれている?)

```
-spec callback(reference(), any(), state()) -> {ok, state()}.
callback(_Ref, _Msg, State) ->
    {ok, State}.
```

## riak_kv_eleveldb_backend:close
`eleveldb:close`を呼び出しているだけ。

```
stop(State) ->
    case State#state.ref of
        undefined ->
            ok;
        _ ->
            eleveldb:close(State#state.ref)
    end,
    ok.
```

## riak_kv_eleveldb_backend:get

`bitcask:get`が`eleveldb:get`に変わったくらいで、`riak_kv_bitcask_backend:get`とほぼ同じ。

```
get(Bucket, Key, #state{read_opts=ReadOpts,
                        ref=Ref}=State) ->
    StorageKey = to_object_key(Bucket, Key),
    case eleveldb:get(Ref, StorageKey, ReadOpts) of
        {ok, Value} ->
            {ok, Value, State};
        not_found  ->
            {error, not_found, State};
        {error, Reason} ->
            {error, Reason, State}
    end.
```

## riak_kv_eleveldb_backend:put
`bitcask:put`と違ってindexに対応しているので、そのための処理が増えている。

Bucket名とキーから実際にデータを書き込む際のキーを作成して、eleveldbに与えるリストUpdate1を作る。

```
put(Bucket, PrimaryKey, IndexSpecs, Val, #state{ref=Ref,
                                                write_opts=WriteOpts}=State) ->
    %% Create the KV update...
    StorageKey = to_object_key(Bucket, PrimaryKey),
    Updates1 = [{put, StorageKey, Val}],
```

## riak_kv_eleveldb_backend:put
IndexSpecsの内容に応じて、putかdeleteでeleveldbに与えるリストUpdate2を作る。

```
    %% Convert IndexSpecs to index updates...
    F = fun({add, Field, Value}) ->
                {put, to_index_key(Bucket, PrimaryKey, Field, Value), <<>>};
           ({remove, Field, Value}) ->
                {delete, to_index_key(Bucket, PrimaryKey, Field, Value)}
        end,
    Updates2 = [F(X) || X <- IndexSpecs],
```

## riak_kv_eleveldb_backend:put
最後にUpdate1とUpdate2の内容を実際にeleveldbに書き込む。

```
    %% Perform the write...
    case eleveldb:write(Ref, Updates1 ++ Updates2, WriteOpts) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.
```

## riak_kv_eleveldb_backend:delete

putがdeleteになるだけで、riak_kv_eleveldb_backend:putと処理の流れはほぼ同じ。

```
delete(Bucket, PrimaryKey, IndexSpecs, #state{ref=Ref,
                                              write_opts=WriteOpts}=State) ->

    %% Create the KV delete...
    StorageKey = to_object_key(Bucket, PrimaryKey),
    Updates1 = [{delete, StorageKey}],

    %% Convert IndexSpecs to index deletes...
    F = fun({remove, Field, Value}) ->
                {delete, to_index_key(Bucket, PrimaryKey, Field, Value)}
        end,
    Updates2 = [F(X) || X <- IndexSpecs],

    case eleveldb:write(Ref, Updates1 ++ Updates2, WriteOpts) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.
```

## riak_kv_eleveldb_backend:fold_buckets

```
fold_buckets(FoldBucketsFun, Acc, Opts, #state{fold_opts=FoldOpts,
                                               ref=Ref}) ->
    FoldFun = fold_buckets_fun(FoldBucketsFun),
    FirstKey = to_first_key(undefined),
    FoldOpts1 = [{first_key, FirstKey} | FoldOpts],
    BucketFolder =
        fun() ->
                try
                    {FoldResult, _} =
                        eleveldb:fold_keys(Ref, FoldFun, {Acc, []}, FoldOpts1),
                    FoldResult
                catch
                    {break, AccFinal} ->
                        AccFinal
                end
        end,
    case lists:member(async_fold, Opts) of
        true ->
            {async, BucketFolder};
        false ->
            {ok, BucketFolder()}
    end.
```

## riak_kv_eleveldb_backend:fold_buckets

eleveldb:fold_keysを呼び出す前にFoldOptsをいじっている。

```
    FirstKey = to_first_key(undefined),
    FoldOpts1 = [{first_key, FirstKey} | FoldOpts],
```

のto_first_keyが、

```
%% @private Given a scope limiter, use sext to encode an expression
%% that represents the starting key for the scope. For example, since
%% we store objects under {o, Bucket, Key}, the first key for the
%% bucket "foo" would be `sext:encode({o, <<"foo">>, <<>>}).`
to_first_key(undefined) ->
    %% Start at the first object in LevelDB...
    to_object_key(<<>>, <<>>);
```

のように定義されているので、ここではLevelDBの最初のオブジェクトからfoldするということ。

## riak_kv_eleveldb_backend:fold_bucket_fun
fold_bucketsが呼び出しているfold_buckets_funは、バイナリのキーを{Backet, Key}のタプルにデコードしつつ、最後のLastBucketに到達するまでFoldBucketsFunでfoldを実行。

```
%% @private
%% Return a function to fold over the buckets on this backend
fold_buckets_fun(FoldBucketsFun) ->
    fun(BK, {Acc, LastBucket}) ->
            case from_object_key(BK) of
                {LastBucket, _} ->
                    {Acc, LastBucket};
                {Bucket, _} ->
                    {FoldBucketsFun(Bucket, Acc), Bucket};
                _ ->
                    throw({break, Acc})
            end
    end.

from_object_key(LKey) ->
    case sext:decode(LKey) of
        {o, Bucket, Key} ->
            {Bucket, Key};
        _ ->
            undefined
    end.
```

## riak_kv_eleveldb_backend:fold_keys
Optionsに`bucket`が指定されていればbucketのkeyを、`index`が指定されていればsecondary indexのkeyに対してfoldを行う。

```
fold_keys(FoldKeysFun, Acc, Opts, #state{fold_opts=FoldOpts,
                                         ref=Ref}) ->
    %% Figure out how we should limit the fold: by bucket, by
    %% secondary index, or neither (fold across everything.)
    Bucket = lists:keyfind(bucket, 1, Opts),
    Index = lists:keyfind(index, 1, Opts),
```

## riak_kv_eleveldb_backend:fold_keys
`bucket`と`index`の両方が指定されている場合がありえるので、その場合はIndexを優先する。

```
    %% Multiple limiters may exist. Take the most specific limiter.
    Limiter =
        if Index /= false  -> Index;
           Bucket /= false -> Bucket;
           true            -> undefined
        end,
```

## riak_kv_eleveldb_backend:fold_keys

```
    %% Set up the fold...
    FirstKey = to_first_key(Limiter),
    FoldFun = fold_keys_fun(FoldKeysFun, Limiter),
    FoldOpts1 = [{first_key, FirstKey} | FoldOpts],
    KeyFolder =
        fun() ->
                %% Do the fold. ELevelDB uses throw/1 to break out of a fold...
                try
                    eleveldb:fold_keys(Ref, FoldFun, Acc, FoldOpts1)
                catch
                    {break, AccFinal} ->
                        AccFinal
                end
        end,
    case lists:member(async_fold, Opts) of
        true ->
            {async, KeyFolder};
        false ->
            {ok, KeyFolder()}
    end.
```

## riak_kv_eleveldb_backend:fold_keys_fun
fold_keys_funは以下のような定義になっていて、Limiterに指定した条件で分岐する。

```
fold_keys_fun(FoldKeysFun, undefined) ->
    %% Fold across everything...
    %% 略
fold_keys_fun(FoldKeysFun, {bucket, FilterBucket}) ->
    %% Fold across a specific bucket...
    %% 略
fold_keys_fun(FoldKeysFun, {index, FilterBucket, {eq, <<"$bucket">>, _}}) ->
    %% 2I exact match query on special $bucket field...
    fold_keys_fun(FoldKeysFun, {bucket, FilterBucket});
fold_keys_fun(FoldKeysFun, {index, FilterBucket, {eq, FilterField, FilterTerm}}) ->
    %% Rewrite 2I exact match query as a range...
    NewQuery = {range, FilterField, FilterTerm, FilterTerm},
    fold_keys_fun(FoldKeysFun, {index, FilterBucket, NewQuery});
fold_keys_fun(FoldKeysFun, {index, FilterBucket, {range, <<"$key">>, StartKey, EndKey}}) ->
    %% 2I range query on special $key field...
    %% 略
fold_keys_fun(FoldKeysFun, {index, FilterBucket, {range, FilterField, StartTerm, EndTerm}}) ->
    %% 2I range query...
    %% 略
```

2IはSecondary Indexの略？

最初の2つの定義は全Keyか特定のBucketの全Keyに対してfoldを実行しているだけ、その次の2つは条件を変えて最後の2つを呼び出しているだけ。

## riak_kv_eleveldb_backend:fold_keys_fun
StartKey以上EndKey以下の範囲にあるKeyに対してfold。

```
fold_keys_fun(FoldKeysFun, {index, FilterBucket, {range, <<"$key">>, StartKey, EndKey}}) ->
    %% 2I range query on special $key field...
    fun(StorageKey, Acc) ->
            case from_object_key(StorageKey) of
                {Bucket, Key} when FilterBucket == Bucket,
                                   StartKey =< Key,
                                   EndKey >= Key ->
                    FoldKeysFun(Bucket, Key, Acc);
                _ ->
                    throw({break, Acc})
            end
    end;
```

## riak_kv_eleveldb_backend:fold_keys_fun
FilterFieldで指定したSecondary IndexががStartTerm以上EndTerm以下の範囲にあるBucketとKeyに対してfold。

```
fold_keys_fun(FoldKeysFun, {index, FilterBucket, {range, FilterField, StartTerm, EndTerm}}) ->
    %% 2I range query...
    fun(StorageKey, Acc) ->
            case from_index_key(StorageKey) of
                {Bucket, Key, Field, Term} when FilterBucket == Bucket,
                                                FilterField == Field,
                                                StartTerm =< Term,
                                                EndTerm >= Term ->
                    FoldKeysFun(Bucket, Key, Acc);
                _ ->
                    throw({break, Acc})
            end
    end;
```

## riak_kv_eleveldb_backend:fold_objects

```
fold_objects(FoldObjectsFun, Acc, Opts, #state{fold_opts=FoldOpts,
                                               ref=Ref}) ->
    Bucket =  proplists:get_value(bucket, Opts),
    FoldOpts1 = fold_opts(Bucket, FoldOpts),
    FoldFun = fold_objects_fun(FoldObjectsFun, Bucket),
    ObjectFolder =
        fun() ->
                try
                    eleveldb:fold(Ref, FoldFun, Acc, FoldOpts1)
                catch
                    {break, AccFinal} ->
                        AccFinal
                end
        end,
    case lists:member(async_fold, Opts) of
        true ->
            {async, ObjectFolder};
        false ->
            {ok, ObjectFolder()}
    end.
```

## riak_kv_eleveldb_backend:fold_objects_fun
バイナリのStorageKeyとオブジェクトValueをうけとって、from_object_keyで{Bucket, Key}にdecodeしつつ、FoldObjectsFunでfoldする。

```
fold_objects_fun(FoldObjectsFun, FilterBucket) ->
    %% 2I does not support fold objects at this time, so this is much
    %% simpler than fold_keys_fun.
    fun({StorageKey, Value}, Acc) ->
            case from_object_key(StorageKey) of
                {Bucket, Key} when FilterBucket == undefined;
                                   Bucket == FilterBucket ->
                    FoldObjectsFun(Bucket, Key, Value, Acc);
                _ ->
                    throw({break, Acc})
            end
    end.
```

## riak_kv_eleveldb_backend:is_empty
自明。

```
is_empty(#state{ref=Ref}) ->
    eleveldb:is_empty(Ref).
```

## riak_kv_eleveldb_backend:status
自明。

```
status(State) ->
    {ok, Stats} = eleveldb:status(State#state.ref, <<"leveldb.stats">>),
    {ok, ReadBlockError} = eleveldb:status(State#state.ref, <<"leveldb.ReadBlockError">>),
    [{stats, Stats}, {read_block_error, ReadBlockError}].
```

## Fin
ご清聴ありがとうございました。
