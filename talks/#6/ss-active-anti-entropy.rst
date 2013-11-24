========================
Active Anti-Entropy: AAE
========================

Riak Source Code Reading @Tokyo #6

:author: Shunichi Shinohara ( @shino / Twitter: @itawasa )
:date: 2013-03-05
:riak: ``3bbaba9`` Merge pull request #283 from basho/jd-moar-nodez
:riak_kv: 3dffe18 Merge pull request #491 from basho/sdc-etoomanylinks

.. contents:: :depth: 2

概観: Riak 1.3 リリースノートより抜粋
=====================================

riak/RELEASE-NOTES.md https://github.com/basho/riak/blob/master/RELEASE-NOTES.md

Riak-1.3-RELEASENOTE.md [日本語訳] https://gist.github.com/kuenishi/5010318

- 1.3 新規導入
- クラスタ内のデータ整合性確認と修繕(repair)
- hash tree を交換する

  - hash tree は LevelDB に永続化、通常の K/V データとは切り離されている
  - それぞれのパーティッションに対して hash tree を生成
  - デフォルトでは、ノードごと、1時間毎にひとつの hash tree を作る
  - 1 時間を超えるともうひとつの hash tree 生成が開始
  - 最大で 2 つの生成まで並列

- 一度 hash tree が作られると write のたびに最新化する

  - 定期的に expire させる、データとの乖離を防ぐため
  - デフォルトでは 1 週間で expire

- ``riak-admin aae-status``

  - 3 セクション: Exchanges, Entropy Trees, Keys Repaired
  - Exchanges は Last: レプリカのひとつとの同期、All: 全レプリカとの同期からの時間
  - Entropy Trees はツリーが作られてからの時間
  - Key Repaired は AAE トリガーの repair 数の最新、平均、最大

- AAE が動き始めるまでには ring_size / number_of_nodes [hours] かかる (例: 128/8 = 16 時間)
- AAE と write がほぼ同時に走ると健全なクラスタでも read repair の可能性がある
- AAE は Riak KV の機能、Riak Search のデータは守らない

参考
----

- 設計から実装まで詳しい説明動画
  http://coffee.jtuple.com/video/AAE.html

動かす
======

4 ノードクラスタで dev3, dev4 を落として、10 オブジェクト PUT 後、元に戻す。

1. riak をクローン
2. app.config をちょっと書き換え: ring を小さくする + AAE がコマメに動くようにする ::

     diff --git a/rel/files/app.config b/rel/files/app.config
     index b70e2ef..e31f6ca 100644
     --- a/rel/files/app.config
     +++ b/rel/files/app.config
     @@ -29 +29 @@
     -              %{ring_creation_size, 64},
     +              {ring_creation_size, 8},            % 8 パーティションでリングを構成
     @@ -100 +100 @@
     -            {anti_entropy_build_limit, {1, 3600000}},
     +            {anti_entropy_build_limit, {1, 10}},  % ハッシュツリーのビルド間隔: 10 msec
     @@ -112 +112 @@
     -            {anti_entropy_concurrency, 2},
     +            {anti_entropy_concurrency, 4},        % ハッシュツリーの並列度
     @@ -119 +119 @@
     -            {anti_entropy_tick, 15000},
     +            {anti_entropy_tick, 1000},            % ハッシュツリーの交換間隔: 1 sec

3. ``$ make stagedevrel``
4. ``$ for i in {1..4}; do echo $i; dev/dev${i}/bin/riak start; done``
5. ``$ for i in {2..4}; do echo $i; dev/dev${i}/bin/riak-admin cluster join dev1@127.0.0.1; done``
6. ``$ dev/dev1/bin/riak-admin cluster plan``
7. ``$ dev/dev1/bin/riak-admin cluster commit``
8. ``$ dev/dev1/bin/riak attach``
9. ``$ for i in {3..4}; do echo $i; dev/dev${i}/bin/riak stop; done``
10. ``for i in {1..10}; do curl -X PUT -d object-${i} http://127.0.0.1:8098/buckets/test/keys/key${i}; done``

ログ抜粋 ::

   2013-03-05 16:13:01.944 [info] <0.1400.0>@riak_kv_exchange_fsm:key_exchange:204 Repaired 1 keys during active anti-entropy exchange of {730750818665451459101842416358141509827966271488,3} between {730750818665451459101842416358141509827966271488,'dev1@127.0.0.1'} and {1096126227998177188652763624537212264741949407232,'dev3@127.0.0.1'}
   2013-03-05 16:13:05.950 [info] <0.1434.0>@riak_kv_exchange_fsm:key_exchange:204 Repaired 2 keys during active anti-entropy exchange of {1096126227998177188652763624537212264741949407232,3} between {0,'dev1@127.0.0.1'} and {1096126227998177188652763624537212264741949407232,'dev3@127.0.0.1'}
   2013-03-05 16:13:42.393 [info] <0.1658.0>@riak_core_handoff_sender:start_fold:130 Starting hinted_handoff transfer of riak_kv_vnode from 'dev1@127.0.0.1' 1278813932664540053428224228626747642198940975104 to 'dev4@127.0.0.1' 1278813932664540053428224228626747642198940975104
   2013-03-05 16:13:42.395 [info] <0.157.0>@riak_core_handoff_manager:handle_info:279 An outbound handoff of partition riak_kv_vnode 1096126227998177188652763624537212264741949407232 was terminated for reason: {shutdown,max_concurrency}
   2013-03-05 16:13:42.411 [info] <0.157.0>@riak_core_handoff_manager:handle_info:279 An outbound handoff of partition riak_kv_vnode 365375409332725729550921208179070754913983135744 was terminated for reason: {shutdown,max_concurrency}
   2013-03-05 16:13:42.460 [info] <0.1658.0>@riak_core_handoff_sender:start_fold:192 hinted_handoff transfer of riak_kv_vnode from 'dev1@127.0.0.1' 1278813932664540053428224228626747642198940975104 to 'dev4@127.0.0.1' 1278813932664540053428224228626747642198940975104 completed: sent 3 objects in 0.07 seconds
   2013-03-05 16:13:42.474 [info] <0.864.0>@riak_core_vnode:handle_info:574 1278813932664540053428224228626747642198940975104 riak_kv_vnode ignored handle_info {'DOWN',#Ref<0.0.0.6178>,process,<0.870.0>,normal} - vnode unregistering
   2013-03-05 16:14:42.397 [info] <0.2012.0>@riak_core_handoff_sender:start_fold:130 Starting hinted_handoff transfer of riak_kv_vnode from 'dev1@127.0.0.1' 548063113999088594326381812268606132370974703616 to 'dev4@127.0.0.1' 548063113999088594326381812268606132370974703616
   2013-03-05 16:14:42.400 [info] <0.2013.0>@riak_core_handoff_sender:start_fold:130 Starting hinted_handoff transfer of riak_kv_vnode from 'dev1@127.0.0.1' 1096126227998177188652763624537212264741949407232 to 'dev3@127.0.0.1' 1096126227998177188652763624537212264741949407232
   2013-03-05 16:14:42.438 [info] <0.2012.0>@riak_core_handoff_sender:start_fold:192 hinted_handoff transfer of riak_kv_vnode from 'dev1@127.0.0.1' 548063113999088594326381812268606132370974703616 to 'dev4@127.0.0.1' 548063113999088594326381812268606132370974703616 completed: sent 4 objects in 0.04 seconds
   2013-03-05 16:14:42.441 [info] <0.2013.0>@riak_core_handoff_sender:start_fold:192 hinted_handoff transfer of riak_kv_vnode from 'dev1@127.0.0.1' 1096126227998177188652763624537212264741949407232 to 'dev3@127.0.0.1' 1096126227998177188652763624537212264741949407232 completed: sent 1 objects in 0.04 seconds
   2013-03-05 16:14:42.453 [info] <0.861.0>@riak_core_vnode:handle_info:574 548063113999088594326381812268606132370974703616 riak_kv_vnode ignored handle_info {'DOWN',#Ref<0.0.0.6213>,process,<0.871.0>,normal} - vnode unregistering
   2013-03-05 16:14:42.461 [info] <0.863.0>@riak_core_vnode:handle_info:574 1096126227998177188652763624537212264741949407232 riak_kv_vnode ignored handle_info {'DOWN',#Ref<0.0.0.6227>,process,<0.872.0>,normal} - vnode unregistering


riak_kv AAE 関連モジュール
==========================

riak_kv_entropy_info
--------------------

- ``riak_admin aae-status`` の受け口

riak_kv_entropy_manager
-----------------------

- gen_server
- 有効化、無効化
- モード設定、デバッグ設定
- exchange 開始

riak_kv_index_hashtree
----------------------

- gen_server
- 一連のハッシュツリーを管理する
- ひとつのハッシュツリーは、特定のパーティションのキー/ハッシュペアから作られる
- ハッシュは直接渡すか ``hash_object(RObjBin)`` を使う

  - ``RObjBin`` は ``riak_object`` の ``term_to_binary`` したもの
  - vclock を正規化して ``term_to_binary`` したものに
  - ``phash2`` をかけて ``term_to_binary`` したものがハッシュ
  - ``erlang:phash2(Term[, Range])``: 戻り値は 0..2^27-1

- riak_kv vnode ひとつにつき ``index_hashtree`` を spawn する
- AAE のために使う

::

   -record(state, {index,
                   vnode_pid,
                   built,
                   lock :: undefined | reference(),
                   path,
                   build_time,
                   trees}).

hashtree
--------

- ディスク永続化のためのハッシュツリー
- docs/hashtree.md をみよ

  https://github.com/basho/riak_kv/blob/master/docs/hashtree.md

- N 分木: N = width
- セグメント: リーフ
- デフォルトでは木の深さ NumLevels =  2 + 1 = 3 ::

    -define(NUM_SEGMENTS, (1024*1024)).
    -define(WIDTH, 1024).
    NumLevels = erlang:trunc(math:log(NumSegments) / math:log(Width)) + 1,

- 新しい ``{Key, Hash}`` を insert すると、セグメントが dirty とマークされる
- ``update_tree`` が呼ばれると dirty なセグメントから上へ再計算が走る
- 永続化は LevelDB
- LevelDB のキーは ``<<$s, SegmentId:64/integer, Key/binary>>``
- セグメントハッシュの再計算では LevelDB イテレータを使う
- オンメモリに木全体を持つことも可能
- キーの個数が 1B のとき、デフォルト設定ではセグメントあたり O(1000) のキー/ハッシュが入る
  ひとつのキーが違うと、ルートからそのセグメントまでのハッシュと
  セグメントに含まれる O(1000) のキー/ハッシュが交換される

riak_kv_exchange_fsm
--------------------

- 交換の処理を行う FSM
- ``riak_kv_entropy_manager`` が start する

ソースコード
============

トークン初期化とタイマーセット
------------------------------

``riak_kv_entropy_manager:init/1``::

   init([]) ->
       schedule_tick(),              % <========
       {_, Opts} = settings(),
       Mode = case proplists:is_defined(manual, Opts) of
                  true ->
                      manual;
                  false ->
                      automatic
              end,
       set_debug(proplists:is_defined(debug, Opts)),
       State = #state{mode=Mode,
                      trees=[],
                      tree_queue=[],
                      locks=[],
                      exchanges=[],
                      exchange_queue=[]},
       State2 = reset_build_tokens(State),  % <========
       schedule_reset_build_tokens(),       % <========
       {ok, State2}.

``riak_kv_entropy_manager:schedule_tick/0``::

   schedule_tick() ->
       %% Perform tick every 15 seconds
       DefaultTick = 15000,
       Tick = app_helper:get_env(riak_kv,
                                 anti_entropy_tick,
                                 DefaultTick),
       erlang:send_after(Tick, ?MODULE, tick),     % <========
       ok.

``riak_kv_entropy_manager:reset_build_tokens/1``::

   -define(DEFAULT_BUILD_LIMIT, {1, 3600000}). %% Once per hour
   %%                      Tokens^  ^^^^^^^ 1 hour

   reset_build_tokens(State) ->
       {Tokens, _} = app_helper:get_env(riak_kv, anti_entropy_build_limit,
                                        ?DEFAULT_BUILD_LIMIT),
       State#state{build_tokens=Tokens}.

``riak_kv_entropy_manager:schedule_reset_build_tokens/0``::

   schedule_reset_build_tokens() ->
       {_, Reset} = app_helper:get_env(riak_kv, anti_entropy_build_limit,
                                       ?DEFAULT_BUILD_LIMIT),
       erlang:send_after(Reset, self(), reset_build_tokens).   % <========

タイマートリガーのリセット
--------------------------

``riak_kv_entropy_manager:handle_info/2``::

   handle_info(reset_build_tokens, State) ->
       State2 = reset_build_tokens(State),
       schedule_reset_build_tokens(),
       {noreply, State2};


tick トリガーの Exchange 起動
-----------------------------

``riak_kv_entropy_manager:handle_info/2``::

   handle_info(tick, State) ->
       State2 = maybe_tick(State),
       {noreply, State2};

``riak_kv_entropy_manager:maybe_tick/1``::

   maybe_tick(State) ->
       case enabled() of
           true ->
               case riak_core_capability:get({riak_kv, anti_entropy}, disabled) of
                   disabled ->
                       NextState = State;
                   enabled_v1 ->
                       NextState = tick(State)            % <========
               end;
           false ->
               %% Ensure we do not have any running index_hashtrees, which can
               %% happen when disabling anti-entropy on a live system.
               [riak_kv_index_hashtree:stop(T) || {_,T} <- State#state.trees],
               NextState = State
       end,
       schedule_tick(),
       NextState.

``riak_kv_entropy_manager::tick/1``::

   tick(State) ->
       {ok, Ring} = riak_core_ring_manager:get_my_ring(),
       State2 = maybe_reload_hashtrees(Ring, State),
       State3 = lists:foldl(fun(_,S) ->
                                    maybe_poke_tree(S)
                            end, State2, lists:seq(1,10)),
       State4 = maybe_exchange(Ring, State3),              % <========
       State4.

``riak_kv_entropy_manager:maybe_exchange/2``::

   maybe_exchange(Ring, State) ->
       case next_exchange(Ring, State) of
           {none, State2} ->
               State2;
           {NextExchange, State2} ->
               {LocalIdx, RemoteIdx, IndexN} = NextExchange,
               case already_exchanging(LocalIdx, State) of
                   true ->
                       requeue_exchange(LocalIdx, RemoteIdx, IndexN, State2);
                   false ->
                       LocalVN = {LocalIdx, node()},
                       case start_exchange(LocalVN, {RemoteIdx, IndexN}, Ring, State2) of          % <========
                           {ok, State3} ->
                               State3;
                           {_Reason, State3} ->
                               State3
                       end
               end
       end.

``riak_kv_entropy_manager:start_exchange/4``::

   start_exchange(LocalVN, {RemoteIdx, IndexN}, Ring, State) ->
       Owner = riak_core_ring:index_owner(Ring, RemoteIdx),
       Nodes = lists:usort([node(), Owner]),
       DownNodes = Nodes -- riak_core_node_watcher:nodes(riak_kv),
       case DownNodes of
           [] ->
               RemoteVN = {RemoteIdx, Owner},
               start_exchange(LocalVN, RemoteVN, IndexN, Ring, State);           % <========
           _ ->
               {{riak_kv_down, DownNodes}, State}
       end.

``riak_kv_entropy_manager:start_exchange/5``::

   start_exchange(LocalVN, RemoteVN, IndexN, Ring, State) ->
       {LocalIdx, _} = LocalVN,
       {RemoteIdx, _} = RemoteVN,
       case riak_core_ring:index_owner(Ring, LocalIdx) == node() of
           false ->
               %% No longer owner of this partition, ignore exchange
               {not_responsible, State};
           true ->
               case orddict:find(LocalIdx, State#state.trees) of
                   error ->
                       %% The local vnode has not yet registered it's
                       %% index_hashtree. Likewise, the vnode may not even
                       %% be running (eg. after a crash).  Send request to
                       %% the vnode to trigger on-demand start and requeue
                       %% exchange.
                       riak_kv_vnode:request_hashtree_pid(LocalIdx),
                       State2 = requeue_exchange(LocalIdx, RemoteIdx, IndexN, State),
                       {not_built, State2};
                   {ok, Tree} ->
                       case riak_kv_exchange_fsm:start(LocalVN, RemoteVN,
                                                       IndexN, Tree, self()) of          % <========
                           {ok, FsmPid} ->
                               Ref = monitor(process, FsmPid),
                               Exchanges = State#state.exchanges,
                               Exchanges2 = [{LocalIdx, Ref, FsmPid} | Exchanges],
                               {ok, State#state{exchanges=Exchanges2}};
                           {error, Reason} ->
                               {Reason, State}
                       end
               end
       end.

exchange FSM
------------


``riak_kv_exchange_fsm:start/5``::

   start(LocalVN, RemoteVN, IndexN, Tree, Manager) ->
       gen_fsm:start(?MODULE, [LocalVN, RemoteVN, IndexN, Tree, Manager], []).

``riak_kv_exchange_fsm:init/1``::

   init([LocalVN, RemoteVN, IndexN, LocalTree, Manager]) ->
       Timeout = app_helper:get_env(riak_kv,
                                    anti_entropy_timeout,
                                    ?DEFAULT_ACTION_TIMEOUT),
       monitor(process, Manager),
       monitor(process, LocalTree),
       State = #state{local=LocalVN,
                      remote=RemoteVN,
                      index_n=IndexN,
                      local_tree=LocalTree,
                      timeout=Timeout,
                      built=0},
       gen_fsm:send_event(self(), start_exchange),
       lager:debug("Starting exchange: ~p", [LocalVN]),
       {ok, prepare_exchange, State}.

``riak_kv_exchange_fsm`` の遷移::

   |------------------+------------------------------------+------------------+-----------------+---------------+--------------+------------------------+----------------|
   |                  | ステート説明                       | start_exchange   | not_responsible | tree_built    | timeout      | {remote_exchange, Pid} | remote_exchage |
   |------------------+------------------------------------+------------------+-----------------+---------------+--------------+------------------------+----------------|
   | prepare_exchange | ロックを取るためのステート         | prepare_exchange |                 |               | stop, normal | update_trees           | stop, normal   |
   |                  |                                    | or stop, normal  |                 |               |              |                        |                |
   |------------------+------------------------------------+------------------+-----------------+---------------+--------------+------------------------+----------------|
   | update_trees     | ロックが取れたのでツリーを更新する |                  | stop, normal    | update_tree   |              | ローカル               |                |
   |                  |                                    |                  |                 | key_kexchange |              |                        |                |
   |------------------+------------------------------------+------------------+-----------------+---------------+--------------+------------------------+----------------|
   | key_exchange     |                                    |                  |                 |               | stop, normal |                        |                |
   |------------------+------------------------------------+------------------+-----------------+---------------+--------------+------------------------+----------------|

``riak_kv_exchange_fsm:key_exchange/2``::

  key_exchange(timeout, State=#state{local=LocalVN,
                                     remote=RemoteVN,
                                     local_tree=LocalTree,
                                     remote_tree=RemoteTree,
                                     index_n=IndexN}) ->
      %% [snip]
      AccFun = fun(KeyDiff, Acc) ->
                       lists:foldl(fun(Diff, Acc2) ->
                                           read_repair_keydiff(RC, LocalVN, RemoteVN, Diff),    % <======
                                           case Acc2 of
                                               [] ->
                                                   [1];
                                               [Count] ->
                                                   [Count+1]
                                           end
                                   end, Acc, KeyDiff)
               end,
      %% TODO: Add stats for AAE
      case riak_kv_index_hashtree:compare(IndexN, Remote, AccFun, LocalTree) of
          [] ->
              exchange_complete(LocalVN, RemoteVN, IndexN, 0),
              ok;
          [Count] ->
              exchange_complete(LocalVN, RemoteVN, IndexN, Count),
              lager:info("Repaired ~b keys during active anti-entropy exchange "
                         "of ~p between ~p and ~p",
                         [Count, IndexN, LocalVN, RemoteVN])
      end,
      {stop, normal, State}.

``riak_kv_exchange_fsm:read_repair_keydiff/2``::

  read_repair_keydiff(RC, LocalVN, RemoteVN, {_, KeyBin}) ->
      {Bucket, Key} = binary_to_term(KeyBin),
      %% [snip]
      RC:get(Bucket, Key),
      %% Force vnodes to update AAE tree in case read repair wasn't triggered
      riak_kv_vnode:rehash([LocalVN, RemoteVN], Bucket, Key),
      ok.
