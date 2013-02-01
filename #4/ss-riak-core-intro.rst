==============
riak_core 入門
==============

Riak Source Code Reading @Tokyo #4

:author: Shunichi Shinohara ( @shino / Twitter: @itawasa )
:date: 2013-02-01
:riak_core: ``60159d5`` "Merge branch '1.3'"


.. contents:: :depth: 2

概観
====

- リポジトリ: https://github.com/basho/riak_core
- ライセンス: Apache License 2.0

- Amazon's Dynamo とのマッピング

  http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html

  1. partitionaing w/ consistent hashing
  2. sloppy membership management w/ gossip protocol
  3. high availability and durability w/ hinted handoff
  4. conflict detection w/ vector clocks
  5. quorum w/ (n, r, w)
  6. anti-entropy w/ markle tree & equal-sized partition


  - 1, 2, 3 を riak_core が提供
  - 一方 4, 5 は riak_kv の範疇
  - 6 は

    - active anti-entropy は 1.3 で登場!
    - markle tree を使っているところは見当たらない
    - equal-sized partitions は riak_core で提供

- 依存

  - rebar.config's deps:

    lager, poolboy, protobuffs, basho_stats, riak_sysmon, webmachine, folsom

  - list of deps directory

    basho_stats bear folsom lager meck mochiweb poolboy protobuffs riak_sysmon webmachine

- riak_core 専用 ML

  http://lists.basho.com/pipermail/riak-core_lists.basho.com/

- riak_core の上のアプリケーションの例: project-fifo/howl · GitHub

  - https://github.com/project-fifo/howl
  - "Howl is a riak_core and web-socket based message delivery system."


riak_core rebar テンプレート
============================

ソースに潜る前に簡単に動かす。

riak_core 上で直に動かしたいアプリのための rebar テンプレートを利用する。

- https://github.com/basho/rebar_riak_core

生成、ビルド、起動、クラスタ構成
--------------------------------

手順::

   $ mkdir ~/tmp/
   $ cd ~/tmp
   $ git clone https://github.com/basho/rebar_riak_core
   $ mkdir firstapp

   $ /path/to/rebar create \
         template=riak_core appid=firstapp \
         template_dir=../rebar_riak_core

   $ cp /path/to/rebar .
   $ make stagedevrel   # 開発者むけ (通常は $ make rel)

シングルノード、シェル付きで起動::

  $ ./dev/dev1/bin/firstapp console
  > q().  #   一旦落とす

マルチノード、シェルなし起動::

  $ for d in dev/dev*; do $d/bin/firstapp start; done
  $ for d in dev/dev*; do echo -n $d ': '; $d/bin/firstapp ping; done
  $ for d in dev/dev{2,3,4}; do $d/bin/firstapp-admin join firstapp1@127.0.0.1; done
  $ ./dev/dev1/bin/firstapp-admin ringready
  TRUE All nodes agree on the ring ['firstapp1@127.0.0.1','firstapp2@127.0.0.1',
                                    'firstapp3@127.0.0.1','firstapp4@127.0.0.1']
  $ ./dev/dev1/bin/firstapp-admin member_status
  ================================= Membership ==================================
  Status     Ring    Pending    Node
  -------------------------------------------------------------------------------
  valid      25.0%      --      'firstapp1@127.0.0.1'
  valid      25.0%      --      'firstapp2@127.0.0.1'
  valid      25.0%      --      'firstapp3@127.0.0.1'
  valid      25.0%      --      'firstapp4@127.0.0.1'
  -------------------------------------------------------------------------------
  Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

ping by HTTP
------------

cURL で ping::

   $ curl http://localhost:10018/firstapp/ping
   <html><head><title>firstapp</title></head><body>
     Result: {pong,1438665674247607560106752257205091097473808596992}
   </body></html>

何度か叩いてみる::

   $ for i in {1..5}; do curl http://localhost:10018/firstapp/ping; echo; done
   [snip]Result: {pong,822094670998632891489572718402909198556462055424}</body></html>
   [snip]Result: {pong,913438523331814323877303020447676887284957839360}</body></html>
   [snip]Result: {pong,433883298582611803841718934712646521460354973696}</body></html>
   [snip]Result: {pong,959110449498405040071168171470060731649205731328}</body></html>
   [snip]Result: {pong,1233142006497949337234359077604363797834693083136}</body></html>

コードを追ってみると

- firstapp_app:start/2::

     EntryRoute = {["firstapp", "ping"], firstapp_wm_ping, []},
     webmachine_router:add_route(EntryRoute),

- firstapp_wm_ping:to_html/2 ::

     Result = io_lib:format("Result: ~p", [firstapp:ping()]),
     {"<html><head><title>firstapp</title></head><body>"
       ++ Result ++ "</body></html>", ReqData, Context}.

- firstapp:ping/0 ::

     %% @doc Pings a random vnode to make sure communication is functional
     ping() ->
         DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
         PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, firstapp),
         [{IndexNode, _Type}] = PrefList,
         riak_core_vnode_master:sync_spawn_command(IndexNode, ping, firstapp_vnode_master).

この流れで使われている riak_core の関数を覗いてみる。

- DocIdx の取得

  ``DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),``

  riak_core_util:chash_key/1 ::

     %% @spec chash_key(BKey :: riak_object:bkey()) -> chash:index()
     %% @doc Create a binary used for determining replica placement.
     chash_key({Bucket,Key}) ->
         BucketProps = riak_core_bucket:get_bucket(Bucket),
         {chash_keyfun, {M, F}} = proplists:lookup(chash_keyfun, BucketProps),
         M:F({Bucket,Key}).

  - riak_kv ではバケット、キーのペアからハッシュ値(リング上の位置)を決める関数
  - バケツごとに異なる関数をつかえるみたい

  chash_keyfun デフォルト値は crypto:sha/1 (SHA) ::

     riak_core_app.erl:73:       {chash_keyfun, {riak_core_util, chash_std_keyfun}}]),

  riak_core_util:chash_std_keyfun/1::

     chash_std_keyfun({Bucket, Key}) -> chash:key_of({Bucket, Key}).

  chash:key_of/1 ::

     key_of(ObjectName) ->
         crypto:sha(term_to_binary(ObjectName)).

  実行例::

     (firstapp1@127.0.0.1)8> riak_core_util:chash_key({<<"mybucket">>, <<"mykey">>}).
     <<120,217,86,93,41,24,68,108,204,109,59,79,211,38,52,1,157,3,124,58>>

- preference list の取得

  ``PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, firstapp),``

  riak_core_apl:get_primary_apl/3 ::

     %% Same as get_apl, but returns only the primaries.
     -spec get_primary_apl(binary(), n_val(), atom()) -> preflist2().
     get_primary_apl(DocIdx, N, Service) ->
         {ok, Ring} = riak_core_ring_manager:get_my_ring(),
         get_primary_apl(DocIdx, N, Ring, riak_core_node_watcher:nodes(Service)).

     %% Same as get_apl, but returns only the primaries.
     -spec get_primary_apl(binary(), n_val(), ring(), [node()]) -> preflist2().
     get_primary_apl(DocIdx, N, Ring, UpNodes) ->
         UpNodes1 = ordsets:from_list(UpNodes),
         Preflist = riak_core_ring:preflist(DocIdx, Ring),
         {Primaries, _} = lists:split(N, Preflist),
         {Up, _} = check_up(Primaries, UpNodes1, [], []),
         Up.

  riak_core_ring:preflist/2 ::

     preflist(Key, State) -> chash:successors(Key, State?CHSTATE.chring).

  chash:successors/2 ::

     %% @doc Given an object key, return all NodeEntries in order starting at Index.
     -spec successors(Index :: index(), CHash :: chash()) -> [node_entry()].
     successors(Index, CHash) ->
         {NumPartitions, _Nodes} = CHash,
         successors(Index, CHash, NumPartitions).

     %% @doc Given an object key, return the next N NodeEntries in order
     %%      starting at Index.
     -spec successors(Index :: index(), CHash :: chash(), N :: integer())
                     -> [node_entry()].
     successors(Index, CHash, N) ->
         Num = max_n(N, CHash),
         Ordered = ordered_from(Index, CHash),
         {NumPartitions, _Nodes} = CHash,
         if Num =:= NumPartitions ->
             Ordered;
            true ->
             {Res, _} = lists:split(Num, Ordered),
             Res
         end.

     %% @doc Return either N or the number of partitions in the ring, whichever
     %%      is lesser.
     -spec max_n(N :: integer(), CHash :: chash()) -> integer().
     max_n(N, {NumPartitions, _Nodes}) ->
         erlang:min(N, NumPartitions).

     %% @doc Given an object key, return all NodeEntries in order starting at Index.
     -spec ordered_from(Index :: index(), CHash :: chash()) -> [node_entry()].
     ordered_from(Index, {NumPartitions, Nodes}) ->
         <<IndexAsInt:160/integer>> = Index,
         Inc = ring_increment(NumPartitions),
         {A, B} = lists:split((IndexAsInt div Inc)+1, Nodes),
         B ++ A.

     -define(RINGTOP, trunc(math:pow(2,160)-1)).  % SHA-1 space

     %% @doc Return increment between ring indexes given
     %% the number of ring partitions.
     -spec ring_increment(NumPartitions :: pos_integer()) -> pos_integer().
     ring_increment(NumPartitions) ->
         ?RINGTOP div NumPartitions.


  実行例::

     (firstapp1@127.0.0.1)19> riak_core_apl:get_primary_apl(DocIdx, 1, firstapp).
     [{{707914855582156101004909840846949587645842325504, 'firstapp4@127.0.0.1'},
       primary}]

     (firstapp1@127.0.0.1)24> riak_core_apl:get_primary_apl(DocIdx, 1, ham).
     []

スーパーバイザーツリー
----------------------

::

   $ ./dev/dev1/bin/firstapp attach
   > observer:start().

firstapp application は単純 ::

  fristapp_sup
       |
       +------------ fristapp_vnode_master

riak_core ::

   application_master (current function: application_master:main_loop/2)
     application_master (current function: application_master:loop_it/4)
       riak_core_sup
         (no name) '$initial_call': mochiweb_socket_server:init/1
         riak_core_capability
         riak_core_claimant
         riak_core_eventhandler_sup
         riak_core_gossip
         riak_core_handoff_sup
         riak_core_node_watcher
         riak_core_node_watcher_events
         riak_core_ring_events
         riak_core_ring_manager
         riak_core_stat_sup
         riak_core_sysmon_minder
         riak_core_vnode_manager
         riak_core_vnode_proxy_sup
           proxy_firstapp_vnode_* が 64 個 '$initial_call': riak_core_vnode_proxy:init/1
                                           current function: riak_core_vnode_proxy:loop/2
         riak_core_vnode_sup
           (no name) が 16 個 '$initial_call': riak_core_vnode:init/1

蛇足: kernel/application_master のコメント (抜粋)::

    %%% The logical and physical process structrure is as follows:
    %%%
    %%%         logical                physical
    %%%
    %%%         --------               --------
    %%%         |AM(GL)|               |AM(GL)|
    %%%         --------               --------
    %%%            |                       |
    %%%         --------               --------
    %%%         |Appl P|               |   X  |
    %%%         --------               --------
    %%%                                    |
    %%%                                --------
    %%%                                |Appl P|
    %%%                                --------
    %%%
    %%% Where AM(GL) == Application Master (Group Leader)
    %%%       Appl P == The application specific root process (child to AM)
    %%%       X      == A special 'invisible' process
    %%% The reason for not using the logical structrure is that
    %%% the application start function is synchronous, and
    %%% that the AM is GL.  This means that if AM executed the start
    [snip]


ソースコードちら見
------------------

firstapp_app:start/2 ::

   start(_StartType, _StartArgs) ->
       case firstapp_sup:start_link() of
           {ok, Pid} ->
               ok = riak_core:register([{vnode_module, firstapp_vnode}]),

               ok = riak_core_ring_events:add_guarded_handler(firstapp_ring_event_handler, []),
               ok = riak_core_node_watcher_events:add_guarded_handler(firstapp_node_event_handler, []),
               ok = riak_core_node_watcher:service_up(firstapp, self()),

               EntryRoute = {["firstapp", "ping"], firstapp_wm_ping, []},
               webmachine_router:add_route(EntryRoute),

               {ok, Pid};
           {error, Reason} ->
               {error, Reason}
       end.

- firstapp_vnode : -behaviour(riak_core_vnode)

  riak_core_vnode のコールバック宣言 ::

     behaviour_info(callbacks) ->
         [{init,1},
          {handle_command,3},
          {handle_coverage,4},
          {handle_exit,3},
          {handoff_starting,2},
          {handoff_cancelled,1},
          {handoff_finished,2},
          {handle_handoff_command,3},
          {handle_handoff_data,2},
          {encode_handoff_item,2},
          {is_empty,1},
          {terminate,2},
          {delete,1}];

  fristapp_vnode の handle_command の実装 ::

     handle_command(ping, _Sender, State) ->
         {reply, {pong, State#state.partition}, State};
     handle_command(Message, _Sender, State) ->
         ?PRINT({unhandled_command, Message}),
         {noreply, State}.

- firstapp_node_event_handler : gen_event

  ほとんど空

- firstapp_ring_event_handler : gen_event

  ほとんど空


riak_core の中へ
================

モジュール概要1: riak_core*
---------------------------

riak_core*

- riak_core.erl

  一番高レベルのAPI

- riak_core.proto

  PB message type 定義

- riak_core_apl.erl

  APL: active preference list ::

    %% Get active preference list - preference list with secondary nodes
    %% substituted.

  TODO: よく分からない

- riak_core_app.erl

  application callback

- riak_core_bucket.erl

  バケット: プロパティの設定など

- riak_core_capability.erl

  capability 管理

  riak_kv を grep (抜粋) ::

    ./src/riak_kv_index_fsm.erl:60:
        riak_core_capability:get({riak_kv, index_backpressure}, false) == true.
    ./src/riak_kv_app.erl:93:
        riak_core_capability:register({riak_kv, vnode_vclocks},
    ./src/riak_kv_app.erl:100:
        riak_core_capability:register({riak_kv, legacy_keylisting},
    ./src/riak_kv_app.erl:107:
        riak_core_capability:register({riak_kv, listkeys_backpressure},
    ./src/riak_kv_app.erl:114:
        riak_core_capability:register({riak_kv, index_backpressure},
    ./src/riak_kv_app.erl:118:
        riak_core_capability:register({riak_kv, mapred_system},
    ./src/riak_kv_app.erl:125:
        riak_core_capability:register({riak_kv, mapred_2i_pipe},
    ./src/riak_kv_mrc_pipe.erl:545:
        case riak_core_capability:get({riak_kv, mapred_2i_pipe}, false) of
    ./src/riak.erl:125:
        case rpc:call(Node, riak_core_capability, get,
                      [{riak_kv, vnode_vclocks}]) of

- riak_core_cinfo_core.erl

  cluster_info application のコールバック

- riak_core_claim*

  - riak_core_claim.erl
  - riak_core_claim_sim.erl
  - riak_core_claim_util.erl
  - riak_core_claimant.erl  (claimant: 要求する人)
  - riak_core_new_claim.erl (deprecated)

  パーティションのオーナー変更系

- riak_core_config.erl

  app.config へのインターフェイス

- riak_core_console.erl

  bin/riak-admin スクリプトの受け側 (全部ではないっぽい)

  riak-admin (shell script) -> nodetool (escript) -> rpc:call (at Riak node)

- riak_core_coverage*

  - riak_core_coverage_fsm.erl

    2i, pipe などの coverage operation を実行する場合、
    このモジュールの behaviour を実装する。 ::

       behaviour_info(callbacks) ->
           [
            {init, 2},
            {process_results, 2},
            {finish, 2}
           ];

    このモジュール自体は gen_fsm を実装している。

    riak_kv で grep '-behaviour(riak_core_coverage_fsm).'::

       ./src/riak_kv_index_fsm.erl     (2i)
       ./src/riak_kv_buckets_fsm.erl   (listing buckets)
       ./src/riak_kv_keys_fsm.erl      (listing bucket keys)
       ./deps/riak_pipe/src/riak_pipe_qcover_fsm.erl

  - riak_core_coverage_plan.erl

    coverage の minimal set を計算する (らしい)

- riak_core_dtrace.erl

  dynamic trace (dtrace/systemtap) 用ユーティリティ

  riak_core 特定ではなく、汎用。

- riak_core_eventhandler_guard.erl
- riak_core_eventhandler_sup.erl

  なんだろう。。。あとで戻ってくるかも。 TODO

- riak_core_format.erl

  io_lib:format 上のちょっとしたユーティリティ

- riak_core_gen_server.erl

  RabbitMQ の gen_server2 っぽい (けどいまのとは違う)。
  Copyright も LShift になっている。

  冒頭コメントをざっと。

  - R13B-1 の gen_server のコピーに以下の変更を加えたもの
  - 名前を変更
  - selective receive の効率向上

    モジュールコールバックを呼ぶ前に、メッセージキューを内部バッファに流しこんでいる。

  - cast が順序保持
  - 新しい関数 pcall/3,4, pcast/3

    プライオリティ付き

  - init/1 のタイムアウトに {binary, Min} が指定可能

    binary exponential timeout になる。 ??

    また handle_info が {noreply, State, hibernate} を返せる。

- riak_core_gossip.erl
- riak_core_gossip_legacy.erl

  リング情報を時々やりとしてクラスタ内で同期する。
  デフォルトは 1 分に 1 回。

  _legacy ではない方には、トークンを持っている。
  ゴシップのやり取りが多すぎると送るのをやめる。

  ::

     %% Default gossip rate: allow at most 45 gossip messages every 10 seconds
     -define(DEFAULT_LIMIT, {45, 10000}).

- riak_core_handoff*

  - riak_core_handoff_listener.erl
  - riak_core_handoff_listener_sup.erl
  - riak_core_handoff_manager.erl
  - riak_core_handoff_receiver.erl
  - riak_core_handoff_receiver_sup.erl
  - riak_core_handoff_sender.erl
  - riak_core_handoff_sender_sup.erl
  - riak_core_handoff_sup.erl

   ハンドオフ系 略(=TODO)

- riak_core_node_watcher*

  - riak_core_node_watcher*.erl

    - gen_server callback
    - service, node の情報を管理

  - riak_core_node_watcher_events.erl

    gen_event callback

    {service_update, Services} というイベントを処理している TODO: 何?

- riak_core_nodeid.erl

  ``node()`` の crc32のバイナリを返す

- riak_core_priority_queue.erl

  - priority queue
  - original is RabbitMQ

- riak_core_repair.erl

  パーティション修復のときのユーティリティ??

  public api の @doc をいくつか拾うと... ::

     %% @doc Generate a `Filter' fun to use during partition repair.
     %% @doc Generate the hash `Range' for a given `Target' partition and
     %%      `NVal'.

- riak_core_ring*

  - riak_core_ring.erl

    %% @doc riak_core_ring manages a riak node's local view of partition ownership.

  - riak_core_ring_events.erl
  - riak_core_ring_handler.erl
  - riak_core_ring_manager.erl
  - riak_core_ring_util.erl

- riak_core_stat*

  - riak_core_stat.erl
  - riak_core_stat_cache.erl
  - riak_core_stat_calc_proc.erl
  - riak_core_stat_calc_sup.erl
  - riak_core_stat_q.erl
  - riak_core_stat_sup.erl  %% riak_core_sup にぶら下がる sup
  - riak_core_stats_sup.erl %% riak_core_stats_sup にぶら下がる sup

  統計, folsom のラッパーとか。

  c.f. riak_core_stat:stats/0 ::

     stats() ->
         [{ignored_gossip_total, counter},
          {rings_reconciled, spiral},
          {gossip_received, spiral},
          {rejected_handoffs, counter},
          {handoff_timeouts, counter},
          {converge_delay, duration},
          {rebalance_delay, duration}].

- riak_core_status.erl

  - クラスタの状態を取得する API/実装
  - このモジュール自体は状態を持たず、クラスタ構成ノードへ問い合わせる

    export はすべて /0 ::

       -export([ringready/0,
              all_active_transfers/0,
              transfers/0,
              ring_status/0]).

- riak_core_sup.erl

  - riak_core application のトップレベル supervisor
  - supervisor/worker がいっぱいぶら下がる

- riak_core_sysmon*

  - riak_core_sysmon_handler.erl
  - riak_core_sysmon_minder.erl

  TODO: ??

  - cf: riak_sysmon  https://github.com/basho/riak_sysmon

- riak_core_test_util.erl

  略 (TODO)

- riak_core_tracer.erl

  ``dbg`` モジュールをゴニョゴニョしている TODO

- riak_core_util.erl

  %% @doc Various functions that are useful throughout Riak.

- riak_core_vnode*

  - riak_core_vnode.erl

    - riak_core を利用する application は vnode 上で動くためにはコレを実装するとおもう
    - gen_fsm
    - behaviour を定義している

      ::

         behaviour_info(callbacks) ->
             [{init,1},
              {handle_command,3},
              {handle_coverage,4},
              {handle_exit,3},
              {handoff_starting,2},
              {handoff_cancelled,1},
              {handoff_finished,2},
              {handle_handoff_command,3},
              {handle_handoff_data,2},
              {encode_handoff_item,2},
              {is_empty,1},
              {terminate,2},
              {delete,1}];

    - riak_core を利用する application は vnode 上で動くためにはコレを実装するはず

      riak_kv で grep ::

        -behaviour(riak_core_vnode).
          ./src/riak_kv_vnode.erl:26:
          ./deps/riak_pipe/src/riak_pipe_vnode.erl:24:
        -behaviour(riak_core_vnode_worker).
          ./src/riak_kv_worker.erl:27:

      riak_kv_vnode:handle_command 抜粋 ::

         handle_command(?KV_PUT_REQ{bkey=BKey,
                                    object=Object,
                                    req_id=ReqId,
                                    start_time=StartTime,
                                    options=Options},
                        Sender, State=#state{idx=Idx}) ->
             riak_kv_mapred_cache:eject(BKey),
             riak_core_vnode:reply(Sender, {w, Idx, ReqId}),
             UpdState = do_put(Sender, BKey,  Object, ReqId, StartTime, Options, State),
             {noreply, UpdState};

  - riak_core_vnode_manager.erl

    Erlang ノード内の vnode 群の管理

    TODO: service ってなんだ?

  - riak_core_vnode_master.erl

    vnode へのアクセスを仕切る gen_server

    e.g.

    - ``command(Preflist, Msg, VMaster)``
    - ``coverage(Msg, CoverageVNodes, Keyspaces, {Type, Ref, From}, VMaster)``

  - riak_core_vnode_proxy.erl

    cf. Question regarding recent riak_core_vnode_master changes
    http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-December/006984.html
    - 全パーティションに対する proxy process が起動する
    - それぞれの proxy process は supervised and registered
    - どこかの vnode にメッセージを送るには proxy に対して送る

    gen_server の state record ::

      -record(state, {mod, index, vnode_pid, vnode_mref}).

  - riak_core_vnode_proxy_sup.erl

    ::

      {{Mod, Index}, {riak_core_vnode_proxy, start_link, [Mod, Index]},
       permanent, 5000, worker, [riak_core_vnode_proxy]}.

  - riak_core_vnode_sup.erl

    riak_core_vnode を simple_one_for_one で監視する sup

  - riak_core_vnode_worker.erl

    ワーカープールのワーカー

  - riak_core_vnode_worker_pool.erl

    ワーカープール

    riak_core_vnode:vnode_command/3 ::

       vnode_command(Sender, Request, State=#state{index=Index, [snip],
                                                   pool_pid=Pool}) ->
           case Action of
               [snip]
               {async, Work, From, NewModState} ->
                   %% dispatch some work to the vnode worker pool
                   %% the result is sent back to 'From'
                   riak_core_vnode_worker_pool:handle_work(Pool, Work, From),
                   continue(State, NewModState);


- riak_core_web.erl

  webmachine/mochiweb の設定ラッパー

  e.g. ``{riak_core, [... {http, ...}, ...]}`` in app.config

- riak_core_wm_urlmap.erl

  doc だけ抜粋::

     %% @doc This module provides a Webmachine resource that lists the
     %%      URLs for other resources available on this host.

モジュール概要1: riak_core* 以外
--------------------------------

- app_helper.erl

  kernel/application のヘルパー

- bloom.erl

  ブルームフィルタ

- chash.erl

  SHA-1 空間でのコンシステントハッシング

- gen_nb_server.erl

  prim_inet:async_accept するための behaviour

  - init で gen_tcp:listen/2, prim_inet:async_accept
  - handle_info({inet_async, ListSock, ...} で TCP accept を処理

     handle_info({inet_async, ListSock, _Ref, {ok, CliSocket}},
                 #state{cb=Callback, server_state=ServerState}=State) ->
       inet_db:register_socket(CliSocket, inet_tcp),
       case Callback:new_connection(CliSocket, ServerState) of
         {ok, NewServerState} ->
           prim_inet:async_accept(ListSock, -1),
           {noreply, State#state{server_state=NewServerState}};
         {stop, Reason, NewServerState} ->
           {stop, Reason, State#state{server_state=NewServerState}}
       end;

  riak で behaviour を grep するとつぎの 2 つ

  - riak_api/src/riak_api_pb_listener.erl
  - riak_core/src/riak_core_handoff_listener.erl

- json_pp.erl

  JSON の pretty print

- merkerl.erl

  Markle Tree

  cf. ハッシュ木 - Wikipedia
  http://ja.wikipedia.org/wiki/%E3%83%8F%E3%83%83%E3%82%B7%E3%83%A5%E6%9C%A8

- process_proxy.erl

  別プロセスへ send (!) するプロキシプロセス

  riak_core で使われているのは1箇所 riak_core_handoff_listener.erl:56 ::

     init([IpAddr, PortNum, SslOpts]) ->
         register(?MODULE, self()),

         %% This exit() call shouldn't be necessary, AFAICT the VM's EXIT
         %% propagation via linking should do the right thing, but BZ 823
         %% suggests otherwise.  However, the exit() call should fall into
         %% the "shouldn't hurt", especially since the next line will
         %% explicitly try to spawn a new proc that will try to register
         %% the riak_kv_handoff_listener name.
         catch exit(whereis(riak_kv_handoff_listener), kill),
         process_proxy:start_link(riak_kv_handoff_listener, ?MODULE),

         {ok, #state{portnum=PortNum, ipaddr=IpAddr, ssl_opts = SslOpts}}.
            process_proxy:start_link(riak_kv_handoff_listener, ?MODULE),

- supervisor_pre_r14b04.erl

  temporary/simple_one_for_one で child の終了 timeout が効かなくなった問題に対応するモジュール

  riak_core で使われているところ::

     ./src/riak_core_vnode_sup.erl:31:
         supervisor_pre_r14b04:start_child(?MODULE, [Mod, Index, ForwardTo]).
     ./src/riak_core_vnode_sup.erl:34:
         %% We use a custom copy of the supervisor module that is expected to be
         %% part of R14B04 or R15. This includes the patch that allows
         %% simple_one_for_one supervisors to do a controlled shutdown.
         %% This is needed because we need to make sure vnode shutdown triggers
         %% async worker pool shutdown AND blocks waiting for the worker pool to
         %% terminate.
         supervisor_pre_r14b04:start_link({local, ?MODULE},

  cf. Initial attempt at clean vnode shutdown that waits for queued work · 00b882a · basho/riak_core
  https://github.com/basho/riak_core/commit/00b882a882e03df2286a4526105adb843b15fd64

- vclock.erl

  Vector clocks

riak_core.app
-------------

::

   {application, riak_core,
   [
     {description, "Riak Core"},
     {vsn, "1.3.0"},
     {modules, [
                [snip]
               ]},
     {registered, []},
     {included_applications, [folsom]},
     {applications, [
                     kernel,
                     stdlib,
                     lager,
                     sasl,
                     crypto,
                     riak_sysmon,
                     webmachine,
                     os_mon
                    ]},
     {mod, { riak_core_app, []}},
     {env, [
            %% Cluster name
            {cluster_name, "default"},

            %% Default location of ringstate
            {ring_state_dir, "data/ring"},

            %% Default ring creation size.  Make sure it is a power of 2,
            %% e.g. 16, 32, 64, 128, 256, 512 etc
            {ring_creation_size, 64},

            %% Default gossip interval (milliseconds)
            {gossip_interval, 60000},

            %% Target N value
            {target_n_val, 4},

            %% Default claims functions
            {wants_claim_fun, {riak_core_claim, default_wants_claim}},
            {choose_claim_fun, {riak_core_claim, default_choose_claim}},

            %% Vnode inactivity timeout (how often to check if fallback vnodes
            %% should return their data) in ms.
            {vnode_inactivity_timeout, 60000},

            %% Number of VNodes allowed to do handoff concurrently.
            {handoff_concurrency, 2},

            %% Disable Nagle on HTTP sockets
            {disable_http_nagle, true},

            %% Handoff IP/port
            {handoff_port, 8099},
            {handoff_ip, "0.0.0.0"}
           ]}
    ]}.


付録
====

firstapp で riak_core を grep
-----------------------------

::

   ./firstapp_console.erl:10:        case riak_core:join(NodeStr) of
   ./firstapp_console.erl:42:        case riak_core:leave() of
   ./firstapp_console.erl:68:        case riak_core:remove(list_to_atom(Node)) of
   ./firstapp_console.erl:89:        case riak_core_status:ringready() of
   ./firstapp.erl:3:-include_lib("riak_core/include/riak_core_vnode.hrl").
   ./firstapp.erl:13:    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
   ./firstapp.erl:14:    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, firstapp),
   ./firstapp.erl:16:    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, firstapp_vnode_master).
   ./firstapp_sup.erl:24:                  {riak_core_vnode_master, start_link, [firstapp_vnode]},
   ./firstapp_sup.erl:25:                  permanent, 5000, worker, [riak_core_vnode_master]},
   ./firstapp_app.erl:15:            ok = riak_core:register([{vnode_module, firstapp_vnode}]),
   ./firstapp_app.erl:17:            ok = riak_core_ring_events:add_guarded_handler(firstapp_ring_event_handler, []),
   ./firstapp_app.erl:18:            ok = riak_core_node_watcher_events:add_guarded_handler(firstapp_node_event_handler, []),
   ./firstapp_app.erl:19:            ok = riak_core_node_watcher:service_up(firstapp, self()),
   ./firstapp_vnode.erl:2:-behaviour(riak_core_vnode).
   ./firstapp_vnode.erl:24:    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).




