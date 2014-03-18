# Riak Core: Ring (2/N: N is unknown)

- riak: commit=`f0d02eb` riak-2.0.0pre19
- riak_core: commit=`95d1d84`

## 概要

- riak core ring の第2回

何回かに渡り...
- ノード join/leave, ノード障害時の挙動
- ring metadata とは、どう伝播して、どう reconcile されるか
- ownership handoff 中の coverage operation も関係してくるかな(?)

今日の目標: tainted ring と raw ring

積み残し

- updates of data in raw ring: membership, capabilities
- vclock of nodes (not of vnodes): 再起動、インクレメント、使い道

## Tainted Ring

Tainted Ring とは? いつつかう?

- raw ring を元に
  - app env の `default_bucket_props` が適用され、
  - 同じく app env の `bucket_fixups` が適用されたもの
    (TODO: fixup は mutator で出てきたけど他にもどこか使っているのか?)
- `riak_core_ring_manager:get_my_ring()` は tainted ring を返す
- ノードローカルなビューだと考えれば良さそう
- 理由は、ローリングアップデートの対応だろうと思う

### コードいくつか

`riak_core_ring_manger`

```
init([Mode]) ->
    ...
    State = set_ring(Ring, #state{mode = Mode}),
    ...

set_ring(Ring, State) ->
    set_ring_global(Ring),
    ...

set_ring_global(Ring) ->
    DefaultProps = case application:get_env(riak_core, default_bucket_props) of
    ...
    %% run fixups on the ring before storing it in mochiglobal
    FixedRing = case riak_core:bucket_fixups() of
        [] -> Ring;
        Fixups ->
            Buckets = riak_core_ring:get_buckets(Ring),
            lists:foldl(
                fun(Bucket, AccRing) ->
                        ...
                        MergedProps = riak_core_bucket:merge_props(
                            BucketProps, DefaultProps),
                        ...
                end, Ring, Buckets)
    end,
    %% Mark ring as tainted to check if it is ever leaked over gossip or
    %% relied upon for any non-local ring operations.
    TaintedRing = riak_core_ring:set_tainted(FixedRing),
    ...
    Actions = [{ring, TaintedRing},
               {raw_ring, Ring},
               {id, {Epoch,Id+1}},
               {chashbin, CHBin} | BucketMeta2],
    ...

```

```
get_my_ring() ->
    Ring = case riak_core_mochiglobal:get(?RING_KEY) of
               ets ->
                   case ets:lookup(?ETS, ring) of  %% <=== Tainted
                       [{_, RingETS}] ->
                           RingETS;
                       _ ->
                           undefined
                   end;
               RingMochi ->
                   RingMochi
           end,
    case Ring of
        Ring when is_tuple(Ring) -> {ok, Ring};
        undefined -> {error, no_ring}
    end.
```

```
promote_ring() ->
    {ok, Ring} = get_my_ring(),
    riak_core_mochiglobal:put(?RING_KEY, Ring).
```


`riak_core_ring`

```
set_tainted(Ring) ->
    update_meta(riak_core_ring_tainted, true, Ring).
```

## Raw Ring

- 入口: `init`, `join`
- 利用箇所: ring membership, ownership, capability, handoff
- 使いドコロ: `riak_core` 内部での利用向けだろう

入口

`riak_core_ring_manager`

```
init([Mode]) ->
    setup_ets(Mode),
    Ring = reload_ring(Mode),
    State = set_ring(Ring, #state{mode = Mode}),
    ...
```

```
set_my_ring(Ring) ->
    gen_server:call(?MODULE, {set_my_ring, Ring}, infinity).

handle_call({set_my_ring, RingIn}, _From, State) ->
    Ring = riak_core_ring:upgrade(RingIn),         % #chstate -> #chstate_v2
    State2 = prune_write_notify_ring(Ring, State), % 
    {reply,ok,State2};

%% Persist a new ring file, set the global value and notify any listeners
prune_write_notify_ring(Ring, State) ->
    State2 = prune_write_ring(Ring, State),
    riak_core_ring_events:ring_update(Ring),
    State2.

prune_write_ring(Ring, State) ->
    %% ここに入ってくる Ring は non-tainted を期待している
    riak_core_ring:check_tainted(Ring, "Error: Persisting tainted ring"),
    ok = riak_core_ring_manager:prune_ringfiles(),
    _ = do_write_ringfile(Ring),
    State2 = set_ring(Ring, State),   % <== set_ring に戻る
    State2.
```

`riak_core_ring_manager:set_my_ring/1` 呼び出し側を探す

- `riak_core:standard_join/4`  % standard = 今のデフォルト
- `riak_core_gossip_legacy:handle_cast({reconcile_ring, OtherRing}, ..)`
- `riak_core_gossip_legacy:remove_from_cluster(ExitingNode)`

memo: `riak_core_gossip_legacy.erl`
- Overhaul cluster membership, ring format, and gossip protocol 
  https://github.com/basho/riak_core/commit/c4b80137998359f0db6eec769c0295db70e61739
- 1.0.0 以前、ここでは触れない

ちょっとだけ `riak_core` モジュールを覗いてみる

```
standard_join(Node, Ring, Rejoin, Auto) ->
    {ok, MyRing} = riak_core_ring_manager:get_raw_ring(),
    SameSize = (riak_core_ring:num_partitions(MyRing) =:=
                riak_core_ring:num_partitions(Ring)),
    Singleton = ([node()] =:= riak_core_ring:all_members(MyRing)),
    case {Rejoin or Singleton, SameSize} of
        {false, _} ->
            {error, not_single_node};
        {_, false} ->
            {error, different_ring_sizes};
        _ ->
            GossipVsn = riak_core_gossip:gossip_version(),
            Ring2 = riak_core_ring:add_member(node(), Ring,
                                              node()),
            Ring3 = riak_core_ring:set_owner(Ring2, node()),
            Ring4 =
                riak_core_ring:update_member_meta(node(),
                                                  Ring3,
                                                  node(),
                                                  gossip_vsn,
                                                  GossipVsn),
            {_, Ring5} = riak_core_capability:update_ring(Ring4),
            Ring6 = maybe_auto_join(Auto, node(), Ring5),
            riak_core_ring_manager:set_my_ring(Ring6),
            riak_core_gossip:send_ring(Node, node())
    end.
```

Raw Ring が使われるところ

```
get_raw_ring() ->
    try
        Ring = ets:lookup_element(?ETS, raw_ring, 2),
        {ok, Ring}
```

`riak_core_ring_manager:get_raw_ring*()` 呼び出し箇所 grep

```
riak-admin ring-status
riak_core_status:ring_status()
./riak_core_status.erl:83:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),

riak_core_gossip:handle_cast({send_ring_to, Node}, ...) etc...
./riak_core_gossip.erl:142:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
./riak_core_gossip.erl:211:    {ok, MyRing0} = riak_core_ring_manager:get_raw_ring(),
./riak_core_gossip.erl:260:    {ok, MyRing} = riak_core_ring_manager:get_raw_ring(),
./riak_core_gossip.erl:267:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),

riak_core_capability:update_supported(State) <- handle_info(register|tick,...)
./riak_core_capability.erl:285:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),


riak_core_claimant:handle_call({stage | plan | tick | ...}, ...)
./riak_core_claimant.erl:260:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
./riak_core_claimant.erl:265:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
./riak_core_claimant.erl:638:            {ok, Ring} = riak_core_ring_manager:get_raw_ring(),


riak_core_vnode_manager:init,
riak_core_vnode_manager:handle_cast(force_handoffs,...), handle_info(management_tick,...)
%% forwarding, handoff の抽出、コントロールに使っている
./riak_core_vnode_manager.erl:210:    {ok, Ring, CHBin} = riak_core_ring_manager:get_raw_ring_chashbin(),
./riak_core_vnode_manager.erl:386:    {ok, Ring, CHBin} = riak_core_ring_manager:get_raw_ring_chashbin(),
./riak_core_vnode_manager.erl:410:    {ok, Ring, CHBin} = riak_core_ring_manager:get_raw_ring_chashbin(),

riak_core_ring:ring_ready()
./riak_core_ring.erl:1220:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),

cluster 構成変更まわり(standard_join, remove, down, leave)
./riak_core.erl:102:                    rpc:call(Node, riak_core_ring_manager, get_raw_ring, [])
./riak_core.erl:127:    {ok, MyRing} = riak_core_ring_manager:get_raw_ring(),
./riak_core.erl:176:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
./riak_core.erl:206:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
./riak_core.erl:230:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
./riak_core_vnode_proxy_sup.erl:62:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),

以下はスルー

%% Clain Simulator
./riak_core_claim_sim.erl:119:                {ok, Ring0} = riak_core_ring_manager:get_raw_ring(),
./riak_core_claim_sim.erl:122:                {ok, Ring0} = rpc:call(Claimant, riak_core_ring_manager, get_raw_ring, []),

%% Legacy, < 1.0.0
./riak_core_gossip_legacy.erl:92:    {ok, MyRing} = riak_core_ring_manager:get_raw_ring(),
./riak_core_gossip_legacy.erl:104:    {ok, MyRing0} = riak_core_ring_manager:get_raw_ring(),
./riak_core_gossip_legacy.erl:125:    {ok, MyRing} = riak_core_ring_manager:get_raw_ring(),
./riak_core_gossip_legacy.erl:180:    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
```
