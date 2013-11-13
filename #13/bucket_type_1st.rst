=============================
Bucket Type, Cluster Metadata
=============================

- riak: 2.0-pre5, commit=3bd9938
- riak_core: commit=19aeaba

概要
====

- Bucket, Key の一つ上の名前空間, 複数バケットで設定を共有できる
- Bucket と違ってユーザが作成する必要がある

  ::

     $ riak-admin bucket-type ...

- 機能によっては bucket type 作成が必須 (CRDT, Strong Consistency)
- いくつかのプロパティは設定したら変更できない

リファレンス

- RICON スライド:
  https://speakerdeck.com/jrwest/controlled-epidemics-riaks-new-gossip-protocol-and-metadata-store
- 提案+議論: https://github.com/basho/riak/issues/362

内部的には

- bucket type は Cluster Metadata (`riak_core_metadata`) を使って保管される
- すべてのノードがタイプがないこと、または単一のバージョンを見ることを保証する
- このため、作成を 2 段階に分ける
  - inactive 状態で作られる
  - inactive での作成が全ノードに行き渡ったら activate できる
  - activate が行き渡ったら type を使うことが出来る
- すべての更新操作は riak_core_claimant を通じてシリアライズされる
- 読み込み(get/1)はだれでもできる。


コード Bucket Type create から
==============================

`riak_core/src/riak_core_bucket_type.erl`

::

   -define(DEFAULT_TYPE, <<"default">>). %% タイプなしは default と同じ

   %% default の activate はスルー
   create(?DEFAULT_TYPE, _Props) ->
       {error, default_type};
   create(BucketType, Props) when is_binary(BucketType) ->
       riak_core_claimant:create_bucket_type(BucketType,
                                             riak_core_bucket_props:merge(Props, defaults())).

   %% クラスタ内の claimant に投げる
   riak_core_claimant:create_bucket_type(BucketType, Props) ->
       gen_server:call(claimant(), {create_bucket_type, BucketType, Props}, infinity).

`riak_core/src/riak_core_claimant.erl`

::

   riak_core_claimant:claimant() ->
       {ok, Ring} = riak_core_ring_manager:get_my_ring(),
       {?MODULE, riak_core_ring:claimant(Ring)}.

   -define(BUCKET_TYPE_PREFIX, {core, bucket_types}).

   handle_call({create_bucket_type, BucketType, Props0}, _From, State) ->
       %% 存在確認
       Existing = get_bucket_type(BucketType, undefined, false),
       %% プロパティのチェック
       case can_create_type(BucketType, Existing, Props0) of
           {ok, Props} ->
               InactiveProps = lists:keystore(active, 1, Props, {active, false}),
               ClaimedProps = lists:keystore(claimant, 1, InactiveProps, {claimant, node()}),
               %% Metadata!
               riak_core_metadata:put(?BUCKET_TYPE_PREFIX, BucketType, ClaimedProps),
               {reply, ok, State};
           Error ->
               {reply, Error, State}
       end;

   riak_core_bucket_props:validate(CreateOrUpdate, Bucket, ExistingProps, BucketProps)
       プロパティのチェック

バリデータ、実際に登録されているものをみてみる

::

   > application:get_env(riak_core, bucket_validators).
   {ok,[{riak_kv,riak_kv_bucket}]}

コード: Cluster Metadata (を少しだけ)
=====================================

`riak_core/include/riak_core_metadata.hrl`

いろいろ意味深

::

   -type metadata_prefix()     :: {binary() | atom(), binary() | atom()}.
   -type metadata_key()        :: any().
   -type metadata_context()    :: dvvset:vector().
   -record(metadata_broadcast, {
             pkey  :: metadata_pkey(),
             obj   :: metadata_object()
            }).
   -type metadata_broadcast()  ::  #metadata_broadcast{}.

`riak_core/src/riak_core_metadata.erl`

::

   %% manager にほぼ丸投げ
   riak_core_metadata:get({Prefix, SubPrefix}=FullPrefix, Key, Opts)
     when (is_binary(Prefix) orelse is_atom(Prefix)) andalso
          (is_binary(SubPrefix) orelse is_atom(SubPrefix)) ->
       PKey = prefixed_key(FullPrefix, Key),
       Default = get_option(default, Opts, undefined),
       ResolveMethod = get_option(resolver, Opts, lww),
       case riak_core_metadata_manager:get(PKey) of
           undefined -> Default;
           Existing ->
               maybe_tombstone(maybe_resolve(PKey, Existing, ResolveMethod), Default)
       end.

`riak_core/src/riak_core_metadata_manager.erl`

::

   %% データは ETS と Dets で持つ
   %% metadata_manager_prefixes_ets:
   %% - これはプレフィックスを管理
   %% - プレフィックスごとに名前なしの ETS テーブルを持ち、そのハンドルを管理している
   -define(ETS, metadata_manager_prefixes_ets).
   %% Dets のファイル名 data/cluster_meta/manifest.dets が出来る
   -define(MANIFEST_FILENAME, "manifest.dets").

   riak_core_metadata_manager:init([Opts]) ->
       case data_root(Opts) of
           undefined ->
               {stop, no_data_dir};
           DataRoot ->
               ets:new(?ETS, [named_table]),
               Nodename = proplists:get_value(nodename, Opts, node()),
               State = #state{serverid=Nodename,
                              data_root=DataRoot,
                              iterators=new_ets_tab()},
               init_manifest(State),
               %% TODO: should do this out-of-band from startup so we don't block
               init_from_files(State),
               {ok, State}
       end.

   riak_core_metadata_manager:put({{Prefix, SubPrefix}, _Key}=PKey, Context, ValueOrFun)
     when (is_binary(Prefix) orelse is_atom(Prefix)) andalso
          (is_binary(SubPrefix) orelse is_atom(SubPrefix)) ->
       gen_server:call(?SERVER, {put, PKey, Context, ValueOrFun}, infinity).
   handle_call({put, PKey, Context, ValueOrFun}, _From, State) ->
       {Result, NewState} = read_modify_write(PKey, Context, ValueOrFun, State),
       {reply, Result, NewState};
   ...
   store({FullPrefix, Key}=PKey, Metadata, State) ->
       maybe_init_ets(FullPrefix),
       maybe_init_dets(FullPrefix, State),

       Objs = [{Key, Metadata}],
       Hash = riak_core_metadata_object:hash(Metadata),
       %% ETS 更新
       ets:insert(ets_tab(FullPrefix), Objs),
       %% ハッシュツリー?
       riak_core_metadata_hashtree:insert(PKey, Hash),
       %% Dets 更新
       ok = dets_insert(dets_tabname(FullPrefix), Objs),
       {Metadata, State}.

その他、見たコード
==================

- claimant の選ばれ方
- hashtree exchange の入口、ロックとか
