% Riak Source Code Reading #3
% csakatoku
% 2013-01-08

## 今日のゴール

- Handoffとは何かをざっくり理解する
- Handoff処理の登場人物を整理する

## Hinted Handoffとは

> Hinted handoffとは、隣接したノードが障害のあるノードに代わって一時的にストレージ処理を行うことで、Riakのクラスターでのノードに障害に対処する方法である。障害のあったノードが復旧した際に、隣接したノードによって処理された更新は元のノードに引き渡される。
>
> Hinted handoff is a technique for dealing with node failure in the Riak cluster in which neighboring nodes temporarily take over storage operations for the failed node. When the failed node returns to the cluster, the updates received by the neighboring nodes are handed off to it.
>
> Hinted handoffはRiakの可用性を保証してくれる。あるノードに障害が起こっても、ノードがまだ生きているかのようにリクエストを処理することができる。
>
> Hinted handoff allows Riak to ensure database availability. When a node fails, Riak can continue to handle requests as if the node were still there.

参考。

- http://wiki.apache.org/cassandra/HintedHandoff
- https://github.com/voldemort/voldemort/wiki/Hinted-Handoff

## RiakのHandoffの種類
`riak/deps/riak_core/include/riak_core_handoff.hrl` に定義されている三種類。

```
-type ho_type() :: ownership_handoff | hinted_handoff | repair.
```

## Handoffが実行されるタイミング

- hinted_handoff
    - 停止していたノードが復旧した
- ownership_handoff
    - Clusterにノードが追加された
    - Clusterからノードが削除された
- repair
    - 自信なし
    - http://docs.basho.com/riak/latest/references/appendices/concepts/Replication/#Read-Repair

## Clusterにノードが追加されたケース - 1
`make devrel` を実行後、dev1, dev2, dev3 を起動済みである程度データを投入済みとする。

```
$ cd dev/dev4
$ ./bin/riak start
$ ./bin/riak-admin cluster join dev1@127.0.0.1
```

## Clusterにノードが追加されたケース - 2

```
% ./bin/riak-admin cluster plan
=============================== Staged Changes ================================
Action         Nodes(s)
-------------------------------------------------------------------------------
join           'dev4@127.0.0.1'
-------------------------------------------------------------------------------

NOTE: Applying these changes will result in 1 cluster transition

###############################################################################
                         After cluster transition 1/1
###############################################################################

================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid      34.4%     25.0%    'dev1@127.0.0.1'
valid      32.8%     25.0%    'dev2@127.0.0.1'
valid      32.8%     25.0%    'dev3@127.0.0.1'
valid       0.0%     25.0%    'dev4@127.0.0.1'
-------------------------------------------------------------------------------
Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

Transfers resulting from cluster changes: 21
  6 transfers from 'dev1@127.0.0.1' to 'dev4@127.0.0.1'
  10 transfers from 'dev3@127.0.0.1' to 'dev4@127.0.0.1'
  5 transfers from 'dev2@127.0.0.1' to 'dev3@127.0.0.1'
```

## Clusterにノードが追加されたケース - 3

```
$ ./bin/riak-admin cluster commit
$ ./bin/riak-admin ring-status
================================== Claimant ===================================
Claimant:  'dev1@127.0.0.1'
Status:     up
Ring Ready: false

============================== Ownership Handoff ==============================
Owner:      dev1@127.0.0.1
Next Owner: dev4@127.0.0.1

Index: 981946412581700398168100746981252653831329677312
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1073290264914881830555831049026020342559825461248
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1164634117248063262943561351070788031288321245184
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1255977969581244695331291653115555720016817029120
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1347321821914426127719021955160323408745312813056
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1438665674247607560106752257205091097473808596992
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

-------------------------------------------------------------------------------
Owner:      dev2@127.0.0.1
Next Owner: dev3@127.0.0.1

```

## Clusterからノードを削除したケース - 1

```
 $ ./bin/riak-admin cluster leave
 $ ./bin/riak-admin cluster plan
=============================== Staged Changes ================================
Action         Nodes(s)
-------------------------------------------------------------------------------
leave          'dev2@127.0.0.1'
-------------------------------------------------------------------------------


NOTE: Applying these changes will result in 2 cluster transitions

###############################################################################
                         After cluster transition 1/2
###############################################################################

================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
leaving    25.0%      0.0%    'dev2@127.0.0.1'
valid      25.0%     34.4%    'dev1@127.0.0.1'
valid      25.0%     32.8%    'dev3@127.0.0.1'
valid      25.0%     32.8%    'dev4@127.0.0.1'
-------------------------------------------------------------------------------
Valid:3 / Leaving:1 / Exiting:0 / Joining:0 / Down:0

WARNING: Not all replicas will be on distinct nodes

Transfers resulting from cluster changes: 48
  5 transfers from 'dev2@127.0.0.1' to 'dev1@127.0.0.1'
  5 transfers from 'dev1@127.0.0.1' to 'dev4@127.0.0.1'
  5 transfers from 'dev4@127.0.0.1' to 'dev3@127.0.0.1'
```

## Clusterからノードを削除したケース - 2

```
% ./bin/riak-admin ring-status
================================== Claimant ===================================
Claimant:  'dev1@127.0.0.1'
Status:     up
Ring Ready: false

============================== Ownership Handoff ==============================
Owner:      dev2@127.0.0.1
Next Owner: dev1@127.0.0.1

Index: 1027618338748291114361965898003636498195577569280
  All transfers complete. Waiting for claimant to change ownership.

Index: 1301649895747835411525156804137939564381064921088
  All transfers complete. Waiting for claimant to change ownership.
```

## 停止したノードが復旧したケース - 1

```
$ cd dev/dev3
$ ./bin/riak stop
```

## 停止したノードが復旧したケース - 2
ある程度の量のデータを投入する。

```
$ curl -XPUT -d '{"test":"test"}' -H 'Content-Type: application/json' http://127.0.0.1:8091/riak/bucket/key
```

## 停止したノードが復旧したケース - 3
停止していたノードを復帰させる。

```
$ ./bin/riak start
```

## 停止したノードが復旧したケース - 4

Hinted Handoffの送信者。

```
2013-01-08 13:07:18.811 [info] <0.4509.0>@riak_core_handoff_sender:start_fold:126 Starting hinted_handoff transfer of riak_kv_vnode from 'dev4@127.0.0.1' 22835963083295358096932575511191922182123945984 to 'dev3@127.0.0.1' 22835963083295358096932575511191922182123945984
2013-01-08 13:07:18.977 [debug] <0.4509.0>@riak_core_handoff_sender:start_fold:170 22835963083295358096932575511191922182123945984 riak_kv_vnode Sending final sync
2013-01-08 13:07:19.094 [debug] <0.4509.0>@riak_core_handoff_sender:start_fold:176 22835963083295358096932575511191922182123945984 riak_kv_vnode Final sync received
2013-01-08 13:07:19.094 [info] <0.4509.0>@riak_core_handoff_sender:start_fold:183 hinted_handoff transfer of riak_kv_vnode from 'dev4@127.0.0.1' 22835963083295358096932575511191922182123945984 to 'dev3@127.0.0.1' 22835963083295358096932575511191922182123945984 completed: sent 504 objects in 0.28 seconds
```

復旧したノード。Hinted Handoffの受信側。

```
2013-01-08 13:07:18.812 [info] <0.959.0>@riak_core_handoff_receiver:process_message:99 Receiving handoff data for partition riak_kv_vnode:22835963083295358096932575511191922182123945984
2013-01-08 13:07:19.096 [info] <0.959.0>@riak_core_handoff_receiver:handle_info:69 Handoff receiver for partition 22835963083295358096932575511191922182123945984 exited after processing 504 objects
```

## Hinted Handoff 概要1

http://docs.basho.com/riak/latest/references/appendices/concepts/Replication/#Understanding-replication-by-example

- 3ノードあるとする
- オブジェクトあたり3つのレプリカをもつとする(N=3)
- 8パーティションあるとする(ring_creation_size=8)

## Hinted Handoff 概要2

Ringのパーティションは次のようになっている。

```
(dev1@127.0.0.1)3> {ok,Ring} = riak_core_ring_manager:get_my_ring().
[{0,'dev1@127.0.0.1'},
{182687704666362864775460604089535377456991567872, 'dev2@127.0.0.1'},
{365375409332725729550921208179070754913983135744, 'dev3@127.0.0.1'},
{548063113999088594326381812268606132370974703616, 'dev1@127.0.0.1'},
{730750818665451459101842416358141509827966271488, 'dev2@127.0.0.1'},
{913438523331814323877303020447676887284957839360, 'dev3@127.0.0.1'},
{1096126227998177188652763624537212264741949407232, 'dev1@127.0.0.1'},
{1278813932664540053428224228626747642198940975104, 'dev2@127.0.0.1'}]
```

## Hinted Handoff 概要3
my_bucket/my_keyというキーのハッシュを160ビットのIntegerでとる。

```
(dev1@127.0.0.1)4> DocIdx = riak_core_util:chash_key({<<"my_bucket">>, <<"my_key">>}).
<<183,28,67,173,80,128,26,94,190,198,65,15,27,243,135,127,121,101,255,96>>
(dev1@127.0.0.1)5> <<I:160/integer>> = DocIdx.
<<183,28,67,173,80,128,26,94,190,198,65,15,27,243,135,127,121,101,255,96>>
(dev1@127.0.0.1)6> I.
1045375627425331784151332358177649483819648417632
```

## Hinted Handoff 概要4
`riak_core_ring:preflist/2` でこのハッシュ(DoxIdx)をどのパーティションに保存すべきかを調べる。N=3なので、上から順番に3つを選択する。

```
(node1@127.0.0.1)> Preflist = riak_core_ring:preflist(DocIdx, Ring).
[{1096126227998177188652763624537212264741949407232, 'dev1@127.0.0.1'}, # ここに保存する
{1278813932664540053428224228626747642198940975104, 'dev2@127.0.0.1'}, # ここに保存する
{0, 'dev1@127.0.0.1'}, # ここに保存する
{182687704666362864775460604089535377456991567872, 'dev2@127.0.0.1'},
{365375409332725729550921208179070754913983135744, 'dev3@127.0.0.1'},
{548063113999088594326381812268606132370974703616, 'dev1@127.0.0.1'},
{730750818665451459101842416358141509827966271488, 'dev2@127.0.0.1'},
{913438523331814323877303020447676887284957839360, 'dev3@127.0.0.1'}]
```

## Hinted Handoff 概要5
しかし、`dev2@127.0.0.1` が落ちていたら？

```
(node1@127.0.0.1)> Preflist = riak_core_ring:preflist(DocIdx, Ring).
[{1096126227998177188652763624537212264741949407232, 'dev1@127.0.0.1'}, # ここに保存する
{1278813932664540053428224228626747642198940975104, 'dev2@127.0.0.1'}, # 落ちている
{0, 'dev1@127.0.0.1'}, # ここに保存する
{182687704666362864775460604089535377456991567872, 'dev2@127.0.0.1'}, # 落ちている
{365375409332725729550921208179070754913983135744, 'dev3@127.0.0.1'}, # 次の候補はここ
{548063113999088594326381812268606132370974703616, 'dev1@127.0.0.1'},
{730750818665451459101842416358141509827966271488, 'dev2@127.0.0.1'},
{913438523331814323877303020447676887284957839360, 'dev3@127.0.0.1'}]
```

## Hinted Handoff 概要6
Fallback先の `dev3@127.0.1` に元のパーティション番号をつけて保存する。

```
'dev3@127.0.0.1' ! {put, Object, 1278813932664540053428224228626747642198940975104}
```

ノードとパーティションのプロセスはこのパーティション番号が自分のものではないことを知っているので、本来の所有者 `dev2@127.0.0.1` が復旧したかどうかを定期的に確認し、復旧していたらそのデータを引き渡す。

http://docs.basho.com/riak/latest/references/appendices/concepts/Replication/#Processing-partition-requests

## あたりをつける
Handoffの処理を行うモジュール。

```
% find . -name "*handoff*.*"
./deps/riak_core/include/riak_core_handoff.hrl
./deps/riak_core/src/riak_core_handoff_sup.erl
./deps/riak_core/src/riak_core_handoff_manager.erl
./deps/riak_core/src/riak_core_handoff_sender.erl
./deps/riak_core/src/riak_core_handoff_receiver.erl
./deps/riak_core/src/riak_core_handoff_listener_sup.erl
./deps/riak_core/src/riak_core_handoff_listener.erl
./deps/riak_core/src/riak_core_handoff_sender_sup.erl
./deps/riak_core/src/riak_core_handoff_receiver_sup.erl
```

Handoffの処理のトリガーを引くモジュール。

```
deps/riak_core/src/riak_core_vnode.erl
deps/riak_core/src/riak_core_vnode_manager.erl
```

## riak_core_handoff_sup.erl

`riak/deps/riak_core/src/riak_core_handoff_sup.erl` を確認すると、`riak_core_handoff_sup` がさらに3つのsupervisorと1つのworkerを起動していることが分かる。

```
-define(CHILD(I,Type), {I,{I,start_link,[]},permanent,brutal_kill,Type,[I]}).

init ([]) ->
    {ok,{{one_for_all,10,10},
         [?CHILD(riak_core_handoff_receiver_sup,supervisor),
          ?CHILD(riak_core_handoff_sender_sup,supervisor),
          ?CHILD(riak_core_handoff_listener_sup,supervisor),
          ?CHILD(riak_core_handoff_manager,worker)
         ]}}.
```

senderがHandoff関連のメッセージを送っているプロセス、receiverがそのメッセージを受け取っているプロセス。managerはlistener, receiver, sender, さらにvnodeを仲介するManagerプロセス。

## Handoffの受信処理

- `riak_core_handoff_listener` がリクエストを受ける
- `riak_core_handoff_listener` が `riak_core_handoff_manager` の `add_inbound` を呼び出す
- `add_inbound`が`receive_handoff`を呼び出す
- `receive_handoff`の中で`riak_handoff_receiver`プロセスを起動する
- `riak_handoff_receiver`がvnodeに対してgen_fsmのイベントを送信してHandoffを処理する

## riak_core_handoff_listener_sup.erl

`riak_core_handoff_listener` はgen_server(正確にはgen_nb_server)で、`riak_core_handoff_listener_sup` はpermanentで子プロセスを起動している。

```
-define(CHILD(I,Type), {I,{I,start_link,[]},permanent,brutal_kill,Type,[I]}).

init ([]) ->
    {ok,{{one_for_one,10,10},
         [?CHILD(riak_core_handoff_listener,worker)
         ]}}.
```

## riak_core_handoff_listener:new_connection/2
app.configのhandoff_ip:handoff_portで指定したアドレスにTCP接続があったら `riak_core_handoff_manager:add_inbound` を呼び出す。

```
new_connection(Socket, State = #state{ssl_opts = SslOpts}) ->
    case riak_core_handoff_manager:add_inbound(SslOpts) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Socket, Pid),
            ok = riak_core_handoff_receiver:set_socket(Pid, Socket),
            {ok, State};
        {error, _Reason} ->
            riak_core_stat:update(rejected_handoffs),
            gen_tcp:close(Socket),
            {ok, State}
    end.
```

## riak_core_handoff_manager:add_inbound/1
さらに`receive_handoff`を呼び出す。

```
add_inbound(SSLOpts) ->
    gen_server:call(?MODULE,{add_inbound,SSLOpts}).

handle_call({add_inbound,SSLOpts},_From,State=#state{handoffs=HS}) ->
    case receive_handoff(SSLOpts) of
        {ok,Handoff=#handoff_status{transport_pid=Receiver}} ->
            HS2 = HS ++ [Handoff],
            {reply, {ok,Receiver}, State#state{handoffs=HS2}};
        Error ->
            {reply, Error, State}
    end;
```

## riak_core_handoff_manager:receive_handoff/1
最終的に`riak_handoff_receiver`が起動され、そのモジュールがリクエストを処理している。

```
%% spawn a receiver process
receive_handoff (SSLOpts) ->
    case handoff_concurrency_limit_reached() of
        true ->
            {error, max_concurrency};
        false ->
            {ok,Pid}=riak_core_handoff_receiver_sup:start_receiver(SSLOpts),
            PidM = monitor(process, Pid),

            %% successfully started up a new receiver
            {ok, #handoff_status{ transport_pid=Pid,
                                  transport_mon=PidM,
                                  direction=inbound,
                                  timestamp=os:timestamp(),
                                  mod_src_tgt={undefined, undefined, undefined},
                                  src_node=undefined,
                                  target_node=undefined,
                                  status=[],
                                  stats=dict:new(),
                                  req_origin=none
                                }
            }
    end.
```

## riak_core_handoff_receiver:process_message ?PT_MSG_INIT
`riak_core_handoff_receiver` には互換性のために過去のプロトコルが残っているが、重要そうな処理は `?PT_MSG_INIT` と `?PT_MSG_OBJ` の二つ。

```
process_message(?PT_MSG_INIT, MsgData, State=#state{vnode_mod=VNodeMod}) ->
    <<Partition:160/integer>> = MsgData,
    lager:info("Receiving handoff data for partition ~p:~p", [VNodeMod, Partition]),
    {ok, VNode} = riak_core_vnode_master:get_vnode_pid(Partition, VNodeMod),
    Data = [{mod_src_tgt, {VNodeMod, undefined, Partition}},
            {vnode_pid, VNode}],
    riak_core_handoff_manager:set_recv_data(self(), Data),
    State#state{partition=Partition, vnode=VNode};
```

この処理が実行されると以下のようなログが残る。

```
2013-01-08 13:07:18.812 [info] <0.959.0>@riak_core_handoff_receiver:process_message:99 Receiving handoff data for partition riak_kv_vnode:22835963083295358096932575511191922182123945984
```

## riak_core_handoff_receiver:process_message ?PT_MSG_OBJ

```
process_message(?PT_MSG_OBJ, MsgData, State=#state{vnode=VNode, count=Count}) ->
    Msg = {handoff_data, MsgData},
    case gen_fsm:sync_send_all_state_event(VNode, Msg, 60000) of
        ok ->
            State#state{count=Count+1};
        E={error, _} ->
            exit(E)
    end;
```

gen_fsmなriak_core_vnodeに対して、 `gen_fsm:sync_send_all_state_event` でHandoffされるデータを送信する。

## riak_core_vnode:handle_sync_event

```
handle_sync_event({handoff_data,BinObj}, _From, StateName,
                  State=#state{mod=Mod, modstate=ModState}) ->
    case Mod:handle_handoff_data(BinObj, ModState) of
        {reply, ok, NewModState} ->
            {reply, ok, StateName, State#state{modstate=NewModState},
             State#state.inactivity_timeout};
        {reply, {error, Err}, NewModState} ->
            lager:error("~p failed to store handoff obj: ~p", [Mod, Err]),
            {reply, {error, Err}, StateName, State#state{modstate=NewModState},
             State#state.inactivity_timeout}
    end;
```

`handle_handoff_data/2` がストレージにHandoffのデータを書き込んでいるが、今日は省略。

## riak_core_handoff_receiver:process_message その3

```
handle_info({tcp_closed,_Socket},State=#state{partition=Partition,count=Count}) ->
    lager:info("Handoff receiver for partition ~p exited after processing ~p"
                          " objects", [Partition, Count]),
    {stop, normal, State};
```

TCPの接続が切れたら、receiverのプロセスも終了。以下のようなログが残る。

```
2013-01-08 13:07:19.096 [info] <0.959.0>@riak_core_handoff_receiver:handle_info:69 Handoff receiver for partition 22835963083295358096932575511191922182123945984 exited after processing 504 objects
```

## Handoff送信処理

- `riak_code_vnode_manager`が応答しないノードを検知する
- `riak_core_handoff_manager:add_outbound/4`が呼び出される
- `add_outbound`が`riak_core_handoff_manager:send_handoff/5`を呼び出す
- `send_handoff`が状態に応じて、repairか、hinted_handoff, ownership_handoffのプロセスを起動する

## riak_core_vnodeがtimeout状態に遷移する

```
active(timeout, State=#state{mod=Mod, index=Idx}) ->
    riak_core_vnode_manager:vnode_event(Mod, Idx, self(), inactive),
    continue(State);
```

## riak_code_vnode_manager:maybe_trigger_handoff
riak_code_vnode_managerが必要ならば`riak_core_vnode:handoff`を実行。

```
handle_vnode_event(inactive, Mod, Idx, Pid, State) ->
    maybe_trigger_handoff(Mod, Idx, Pid, State),
    {noreply, State};

maybe_trigger_handoff(Mod, Idx, Pid, _State=#state{handoff=HO}) ->
    case orddict:find({Mod, Idx}, HO) of
        {ok, TargetNode} ->
            riak_core_vnode:trigger_handoff(Pid, TargetNode),
            ok;
        error ->
            ok
    end.
```

## riak_core_vnode:trigger_handoff
すでに同じHandoffが実行されていなければ、`riak_core_vnode:start_handoff`処理を実行。

```
maybe_handoff(State=#state{index=Idx, mod=Mod, modstate=ModState,
                           handoff_node=HN}, TargetNode) ->
    case HN of
        none ->
            ok;
        TargetNode ->
            ok;
        _ ->
            lager:info("~s/~b: handoff request to ~p before "
                       "finishing handoff to ~p", [Mod, Idx, TargetNode, HN])
    end,
    case Mod:handoff_starting(TargetNode, ModState) of
        {true, NewModState} ->
            start_handoff(State#state{modstate=NewModState}, TargetNode);
        {false, NewModState} ->
            continue(State, NewModState)
    end.
```

## riak_core_vnode:start_handoff
`riak_core_handoff_manager:add_outbound` を呼び出す。

```
start_handoff(State=#state{index=Idx, mod=Mod, modstate=ModState}, TargetNode) ->
    case Mod:is_empty(ModState) of
        {true, NewModState} ->
            finish_handoff(State#state{modstate=NewModState,
                                       handoff_node=TargetNode});
        {false, NewModState} ->
            case riak_core_handoff_manager:add_outbound(Mod,Idx,TargetNode,self()) of
                {ok,_Pid} ->
                    NewState = State#state{modstate=NewModState,
                                           handoff_node=TargetNode},
                    continue(NewState);
                {error,_Reason} ->
                    continue(State#state{modstate=NewModState})
            end
    end.
```

## riak_core_handoff_manager:add_outbound
`riak_core_handoff_manager:send_handoff` を呼び出す。

```
add_outbound(Module,Idx,Node,VnodePid) ->
    gen_server:call(?MODULE,{add_outbound,Module,Idx,Node,VnodePid}).

handle_call({add_outbound,Mod,Idx,Node,Pid},_From,State=#state{handoffs=HS}) ->
    case send_handoff(Mod,Idx,Node,Pid,HS) of
        {ok,Handoff=#handoff_status{transport_pid=Sender}} ->
            HS2 = HS ++ [Handoff],
            {reply, {ok,Sender}, State#state{handoffs=HS2}};
        {false,_ExistingHandoff=#handoff_status{transport_pid=Sender}} ->
            {reply, {ok,Sender}, State};
        Error ->
            {reply, Error, State}
    end;
```

## riak_core_handoff_manager:send_handoff その1

```
send_handoff({Mod, Src, Target}, Node, Vnode, HS, {Filter, FilterModFun}, Origin) ->
    case handoff_concurrency_limit_reached() of
        true ->
            {error, max_concurrency};
        false ->
            %% continue
```

上限に達していれば `max_concurrency` エラーで終了。

## riak_core_handoff:send_handoff その2

```
         ShouldHandoff=
                case lists:keyfind({Mod, Src, Target}, #handoff_status.mod_src_tgt, HS) of
                    false ->
                        true;
                    Handoff=#handoff_status{target_node=Node,vnode_pid=Vnode} ->
                        {false,Handoff};
                    #handoff_status{transport_pid=Sender} ->
                        %% found a running handoff with a different vnode
                        %% source or a different arget ndoe, kill the current
                        %% one and the new one will start up
                        erlang:exit(Sender,resubmit_handoff_change),
                        true
                end,
```

- 同じSrcからTargetに対して同一処理(モジュール)が存在しなければ真
- 存在して、Handoff先とHandoff元が同一であればfalse
- 存在して、Handoff先かHandoff元のどちらかが異なれば現在実行中のHandoffを終了させた上で真

## riak_core_handoff:send_handoff その3
すでに実行中のHandoffがあればその情報を返す。

```
            case ShouldHandoff of
                %% handoff already going, just return it
                AlreadyExists={false,_CurrentHandoff} ->
                    AlreadyExists
```

## riak_core_handoff:send_handoff その4
Handoffを実行する必要があるときには、Handoffの種別を決定する。

```
            case ShouldHandoff of
                true ->
                    VnodeM = monitor(process, Vnode),
                    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
                    %% assumes local node is doing the sending
                    Primary = riak_core_ring:is_primary(Ring, {Src, node()}),
                    HOType = if Primary ->
                                     if Src == Target -> ownership_handoff;
                                        true -> repair
                                     end;
                                true -> hinted_handoff
                             end,
```

## riak_core_handoff:send_handoff その5
repairの場合には`start_repair`を、hinted_handoff, ownership_handoffの場合には`start_handoff`を実行。

```
                    %% start the sender process
                    case HOType of
                        repair ->
                            {ok, Pid} =
                                riak_core_handoff_sender_sup:start_repair(Mod, Src, Target, Vnode, Node, Filter);
                        _ ->
                            {ok,Pid} =
                                riak_core_handoff_sender_sup:start_handoff(HOType, Mod, Target, Vnode, Node)
                    end,
                    PidM = monitor(process, Pid),
```

## riak_core_handoff:send_handoff その6
実行中のHandoffの状態を返す。

```
                    %% successfully started up a new sender handoff
                    {ok, #handoff_status{ transport_pid=Pid,
                                          transport_mon=PidM,
                                          direction=outbound,
                                          timestamp=os:timestamp(),
                                          src_node=node(),
                                          target_node=Node,
                                          mod_src_tgt={Mod, Src, Target},
                                          vnode_pid=Vnode,
                                          vnode_mon=VnodeM,
                                          status=[],
                                          stats=dict:new(),
                                          type=HOType,
                                          req_origin=Origin,
                                          filter_mod_fun=FilterModFun
                                        }
```

## riak_core_handoff_sender
Handoffの送信処理を行うためのかなり泥臭いコードが`start_fold`に書かれている。互換性を保つためのコードも混じっているので、さらに泥臭い。

```
         %% Piggyback the sync command from previous releases to send
         %% the vnode type across.  If talking to older nodes they'll
         %% just do a sync, newer nodes will decode the module name.
         %% After 0.12.0 the calls can be switched to use PT_MSG_SYNC
         %% and PT_MSG_CONFIGURE
         VMaster = list_to_atom(atom_to_list(Module) ++ "_master"),
         ModBin = atom_to_binary(Module, utf8),
         Msg = <<?PT_MSG_OLDSYNC:8,ModBin/binary>>,
         ok = TcpMod:send(Socket, Msg),
```

## Fin

ご清聴ありがとうございました。
