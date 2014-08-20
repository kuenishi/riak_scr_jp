# Hinted-Handoff

## Hinted Hanodffとは

- http://docs.basho.com/riak/latest/theory/concepts/glossary/#Hinted-Handoff

```
Hinted handoff is a technique for dealing with node failure in the Riak cluster in which neighboring nodes temporarily take over storage operations for the failed node. When the failed node returns to the cluster, the updates received by the neighboring nodes are handed off to it.

Hinted handoff allows Riak to ensure database availability. When a node fails, Riak can continue to handle requests as if the node were still there.
```

## Riakの様々なHandoff

Riakでは様々なHandoffが存在する。特にソースコード中で定義されているものが4つ。
https://github.com/basho/riak_core/blob/develop/include/riak_core_handoff.hrl

```
-type ho_type() :: ownership_handoff | hinted_handoff | repair | resize_transfer.
```

Riakは本来vnode単位で見ており、vnodeのhandoffは、すなわちそのvnodeが担当している
データのtransferを伴うことになる。これらは riak-admin transfers で表示されるものでもある。


## Hinted Handoffの挙動を実際の環境とソースコードから追いかける

### 環境

- Riak2.0 RC1
- 4ノード (make stagedevrelで構築したうちのdev1,2,3,4 を使用)
- Ring Size : 8
- n_val : 3

```
(dev1@127.0.0.1)1> {ok,Ring} = riak_core_ring_manager:get_my_ring().

{ok,{chstate_v2,'dev1@127.0.0.1',
                [{'dev1@127.0.0.1',{92,63575568059}},
                 {'dev2@127.0.0.1',{52,63575568059}},
                 {'dev3@127.0.0.1',{46,63575568059}},
                 {'dev4@127.0.0.1',{45,63575568051}}],
{8,
  [
    {0,'dev1@127.0.0.1'},
    {182687704666362864775460604089535377456991567872, 'dev2@127.0.0.1'},
    {365375409332725729550921208179070754913983135744, 'dev3@127.0.0.1'},
    {548063113999088594326381812268606132370974703616, 'dev4@127.0.0.1'},
    {730750818665451459101842416358141509827966271488, 'dev1@127.0.0.1'},
    {913438523331814323877303020447676887284957839360, 'dev2@127.0.0.1'},
    {1096126227998177188652763624537212264741949407232, 'dev3@127.0.0.1'},
    {1278813932664540053428224228626747642198940975104, 'dev4@127.0.0.1'}
  ]
},...
```

## clietn から put した時を追いかける

dev1,2,3,4 があり、この時dev2がノードダウンしているとする。
ここで dev1,2,3, に 3レプリカが保存されるようなkeyをput。
なお、クライアントはdev1に接続してきているとする(つまりforwardは考慮しない)

この場合、dev1,2,4に3レプリカが保存され、dev4はfallbackノードという扱いになるはず。

エントリーポイントとして、riak_client:put から追いかける。

### riak_client:put

strong consistency を使った時の分岐。今回は normal_put へ。　

```erlang
put(RObj, Options, {?MODULE, [Node, _ClientId]}=THIS) when is_list(Options) ->
    case consistent_object(Node, riak_object:bucket(RObj)) of
        true ->
            consistent_put(RObj, Options, THIS);
        false ->
            normal_put(RObj, Options, THIS);
        {error,_}=Err ->
            Err
    end;
```

### riak_client:normal_put

dev1上でput_fsmが起動する。

```erlang
riak_kv_put_fsm:start_link({raw, ReqId, Me}, RObj, Options);
```

### riak_kv_put_fsm:start_link ~ init

put_fsmの初期化処理が終わったらprepare stateへ遷移。

```erlang
init([From, RObj, Options0, Monitor]) ->
...略
   {ok, prepare, StateData, 0};
```

### riak_kv_put_fsm:prepare

sloppy_quorum が true、すなわち hinted handoff が動作する場合のコード分岐。
まず、riak_core_node_watcher に現在有効なノードを問い合わせている。
そこから実際に今回問い合わせるPreflistを生成。

```erlang
Preflist2 = 
    case get_option(sloppy_quorum, Options, true) of
        true ->
            UpNodes = riak_core_node_watcher:nodes(riak_kv),
            riak_core_apl:get_apl_ann(DocIdx, N, 
                                      UpNodes -- BadCoordinators);
```
primary か fallback かという情報とともにリストを返している。

```erlang
get_apl_ann(DocIdx, N, UpNodes) ->
    {ok, CHBin} = riak_core_ring_manager:get_chash_bin(),
    get_apl_ann_chbin(DocIdx, N, CHBin, UpNodes).

get_apl_ann_chbin(DocIdx, N, CHBin, UpNodes) ->
    UpNodes1 = UpNodes,
    Itr = chashbin:iterator(DocIdx, CHBin),
    {Primaries, Itr2} = chashbin:itr_pop(N, Itr),
    {Up, Pangs} = check_up(Primaries, UpNodes1, [], []),
    Up ++ find_fallbacks_chbin(Pangs, Itr2, UpNodes1, []).
```

実際にどんな情報が返るかを検証。以下はdev1にriak attachした結果。
まずはbucketとkeyからリング上のどこに格納されるものかのインデックスを算出。
続いて、現在生存しているノード一覧を取得。（以下は全ノードが生きている場合の結果）
```
(dev1@127.0.0.1)1> DocIdx = riak_core_util:chash_key({<<"mybucket">>, <<"key1">>}).
<<107,99,109,62,16,196,1,234,36,164,217,176,99,139,208,34,149,133,214,224>>

(dev1@127.0.0.1)2> UpNodes = riak_core_node_watcher:nodes(riak_kv).
['dev2@127.0.0.1','dev4@127.0.0.1','dev3@127.0.0.1','dev1@127.0.0.1']
```
そして問い合わせ。
通常は全てprimary
```
(dev1@127.0.0.1)3> riak_core_apl:get_apl_ann(DocIdx, 3, UpNodes1).
[{{730750818665451459101842416358141509827966271488,
   'dev1@127.0.0.1'},
     primary},
  {{913438523331814323877303020447676887284957839360,
   'dev2@127.0.0.1'},
     primary},
  {{1096126227998177188652763624537212264741949407232,
   'dev3@127.0.0.1'},
     primary}]
```

dev2を落として実行してみると、dev4が fallbackとして表示される
```
(dev1@127.0.0.1)14> riak_core_apl:get_apl_ann(DocIdx, 3, UpNodes1).
[{{730750818665451459101842416358141509827966271488,
   'dev1@127.0.0.1'},
     primary},
 {{1096126227998177188652763624537212264741949407232,
   'dev3@127.0.0.1'},
     primary},
 {{913438523331814323877303020447676887284957839360,
   'dev4@127.0.0.1'}
     fallback}]
```

Preflistを生成したあとは、
自分(dev1)がPreflistに含まれている場合、
自分のローカル(のvnode)に1つ以上のレプリカデータがあることとになる。
この場合ローカルでコーディネータを起動。
ない場合は、Preflistのノードのうちランダムに選択し、
リモートでコーディネータ起動。

今回はローカルで起動したとして読み進める。

次は validate へ。
```erlang
new_state_timeout(validate, StateData)
```

### riak_kv_put_fsm:validate

PUT時のオプションとか、バケットプロパティとかのバリデーションしてる。
precommit,postcommitとかのhookも。

そしてexecute() へ。
```erlang
case Precommit of
    [] -> % Nothing to run, spare the timing code
        execute(StateData);
    _ ->
        new_state_timeout(precommit, StateData)
```

### riak_kv_put_fsm:execute ~ execute_local ~ waiting_local_vnode
自身がコーディネータの場合と、リモートからフォワードされてコーディネータの場合がある。

```erlang
    case CPL of
        undefined ->
            execute_remote(State);
        _ ->
            execute_local(State)
    end.
```

この時点で、自分がコーディネータであり、
自分とあと他2つ(N-1)のノードにレプリカをputする必要がある。
まずはcoord_putとして、ローカルにput。
そして waiting_local_vnode stateへ。
```erlang
    riak_kv_vnode:coord_put(CoordPLEntry, BKey, RObj, ReqId, StartTime, VnodeOptions),
    StateData2 = StateData1#state{robj = RObj, tref = TRef},
    %% Must always wait for local vnode - it contains the object with updated vclock
    %% to use for the remotes. (Ignore optimization for N=1 case for now).
    new_state(waiting_local_vnode, StateData2).
```

ローカルに書き終わったら、remoteに書く。
```erlang
        {dw, Idx, PutObj, _ReqId} ->
            %% Either returnbody is true or coord put merged with the existing
            %% object and bumped the vclock.  Either way use the returned
            %% object for the remote vnode
            ?DTRACE(Trace, ?C_PUT_FSM_WAITING_LOCAL_VNODE, [2],
                    [integer_to_list(Idx)]),
            execute_remote(StateData#state{robj = PutObj, putcore = UpdPutCore1});
```
### riak_kv_put_fsm:execute_remote ~ waiting_remote_vnode

riak_kv_vnode:put をして、waiting_remote_vnode stateへ。
あとはackを待って、Wだけ書けたら、クライアントに応答を返す。

```erlang
    riak_kv_vnode:put(Preflist, BKey, RObj, ReqId, StartTime, VnodeOptions),
    case riak_kv_put_core:enough(PutCore) of
        true ->
            {Reply, UpdPutCore} = riak_kv_put_core:response(PutCore),
            process_reply(Reply, StateData1#state{putcore = UpdPutCore});
        false ->
            new_state(waiting_remote_vnode, StateData1)
    end.
```



### riak_kv_vnode:put
リモートへ書きに行く。これまでの時点で primary も fallback も差がない。

riak_core_vnode_master:command で KV_PUT_REQ を投げてる。

```erlang
put(Preflist, BKey, Obj, ReqId, StartTime, Options, Sender)
  when is_integer(StartTime) ->
    riak_core_vnode_master:command(Preflist,
                                   ?KV_PUT_REQ{
                                      bkey = sanitize_bkey(BKey),
                                      object = Obj,
                                      req_id = ReqId,
                                      start_time = StartTime,
                                      options = Options},
                                   Sender,
                                   riak_kv_vnode_master).
```

その後

- riak_core_vnode_master:command
- riak_core_vnode_master:command2
- riak_core_vnode_master:proxy_cast
- riak_core_vnode_master:do_proxy_cast
- riak_core_vnode_master:send_an_event
- gen_fsm:send_event

で、リクエストが送信される

例えばこんなリクエスト。
```
[{proxy_riak_kv_vnode_913438523331814323877303020447676887284957839360,
   'dev4@127.0.0.1'},
  {riak_vnode_req_v1,
   913438523331814323877303020447676887284957839360,
   {fsm,undefined,
    <0.25821.28>},...
```



### riak_core_vnode_proxy:handle_proxyなど
リモートサイド。つまり今回の例では dev3,dev4 dev1からのリクエストを受け取る。
vnode_request を送って、受け取って、キューに突っ込んで・・・いろいろ長いので端折る。
ここは riak_core レイヤー。riak_core は汎用的に作られており、上位層からのリクエストは、
vnode request という形でラップされて送られる。
Riakでriak_coreを使う時は、riak_coreの上にはriak_kvやriak_pipe（あと一応riak_search）が
登録されていて、これらはriak_coreを経由して、ノード間でリクエストを送ることができる。

### riak_kv_vnode:handle_command
途中端折って、riak_kvレイヤーに戻ってきて、ここに来るはず。

```erlang
handle_command(?KV_PUT_REQ{bkey=BKey,
                           object=Object,
                           req_id=ReqId,
                           start_time=StartTime,
                           options=Options},
               Sender, State=#state{idx=Idx}) ->
    StartTS = os:timestamp(),
    riak_core_vnode:reply(Sender, {w, Idx, ReqId}),
    UpdState = do_put(Sender, BKey,  Object, ReqId, StartTime, Options, State),
    update_vnode_stats(vnode_put, Idx, StartTS),
    {noreply, UpdState};
```

### riak_kv_vnode:do_put
そして do_put にて実際のput。
ここまでにfallbackノードかprimaryノードかの差異なし（のように見える）
get時の挙動は追いかけていないが、恐らくputと同じように
Preflistを作るときの違いぐらいだと思う。
他の視点でhandoff周りを探してみる。


### riak_core_handoff_manager

これが怪しいので redbug（デバッガ） を仕掛けてみる。
現在 dev2 が down していて、dev4 が fallback として使われている(dev1,2,3 がprimaryのkeyの場合)
dev2 を up させると、以下のログが出力された。

```
17:54:56 <0.24659.29>({erlang,apply,2}) {riak_core_handoff_manager, 
                                         get_exclusions, 
                                         [riak_kv_vnode]} 
  riak_core_vnode_manager:'-ensure_vnodes_started/1-fun-1-'/1
  riak_core_ring_handler:ensure_vnodes_started/1
  riak_core_ring_handler:ensure_vnodes_started/3
  riak_core_ring_handler:ensure_vnodes_started/2

17:54:56 <0.166.0>(riak_core_handoff_manager) {riak_core_handoff_manager,
                                               handle_call,
                                               [{get_exclusions,riak_kv_vnode},
                                                {<0.24659.29>,
                                                 #Ref<0.0.26.163774>},
                                                {state,
                                                 {set,11,16,16,8,80,48,
                                                  {[],[],[],[],[],[],[],[],[],
                                                   [],[],[],[],[],[],[]},
                                                  {{[],[],[],[],[],[],[],
                                                    [{riak_kv_vnode,0},
                                                     {riak_kv_vnode,
                                                      182687704666362864775460604089535377456991567872}, 
                                                     {riak_kv_vnode,
                                                      730750818665451459101842416358141509827966271488}, 
                                                     {riak_kv_vnode,
                                                      365375409332725729550921208179070754913983135744}, 
                                                     {riak_kv_vnode,
                                                      1096126227998177188652763624537212264741949407232},
                                                     {riak_pipe_vnode,
                                                      1096126227998177188652763624537212264741949407232},
                                                     {riak_pipe_vnode,
                                                      365375409332725729550921208179070754913983135744}, 
                                                     {riak_pipe_vnode,
                                                      730750818665451459101842416358141509827966271488}, 
                                                     {riak_pipe_vnode,
                                                      182687704666362864775460604089535377456991567872}, 
                                                     {riak_pipe_vnode,
                                                      913438523331814323877303020447676887284957839360}, 
                                                     {riak_pipe_vnode,0}],                                                    [],[],[],[],[],[],[],[]}}},
                                                 []}]}
  proc_lib:init_p_do_apply/3
```

get_exclusion が呼ばれているが、名前から推測するに exclude するvnode一覧を取得している？
ここで自分が本来持っていないvnode一覧を取得できているように見えるが、
では この情報の登録 add_exclusion はいつしているか？

### riak_core_vnode:active

riak_core_vnode:active の中で、add_exclusion を実行している。

```
active(unregistered, State=#state{mod=Mod, index=Index}) ->
    %% Add exclusion so the ring handler will not try to spin this vnode
    %% up until it receives traffic.
    riak_core_handoff_manager:add_exclusion(Mod, Index),
    lager:debug("~p ~p vnode excluded and unregistered.",
                [Index, Mod]),
    {stop, normal, State#state{handoff_target=none,
                               handoff_type=undefined,
                               pool_pid=undefined}}.
```

これはいつ起こる？

riak-admin cluster join/plan/commit 直後のdev4のログ(debug levelの時のみ)に、関連ログが出ている。
```
2014-08-18 16:22:33.215 [debug] <0.471.0>@riak_core_vnode:finish_handoff:652 0 riak_kv_vnode vnode finished handoff and deleted.
2014-08-18 16:22:33.215 [debug] <0.471.0>@riak_core_vnode:finish_handoff:655 vnode hn/fwd :: riak_kv_vnode/0 :: undefined -> 'dev1@127.0.0.1'
2014-08-18 16:22:33.216 [debug] <0.471.0>@riak_core_vnode:active:533 0 riak_kv_vnode vnode excluded and unregistered

...

2014-08-18 16:22:57.288 [debug] <0.600.0>@riak_core_vnode:finish_handoff:652 730750818665451459101842416358141509827966271488 riak_kv_vnode vnode finished handoff and deleted.
2014-08-18 16:22:57.288 [debug] <0.600.0>@riak_core_vnode:finish_handoff:655 vnode hn/fwd :: riak_kv_vnode/730750818665451459101842416358141509827966271488 :: undefined -> 'dev1@127.0.0.1'
2014-08-18 16:22:57.288 [debug] <0.600.0>@riak_core_vnode:active:533 730750818665451459101842416358141509827966271488 riak_kv_vnode vnode excluded and unregistered.
2014-08-18 16:22:58.716 [debug] <0.597.0>@riak_core_vnode:finish_handoff:652 182687704666362864775460604089535377456991567872 riak_kv_vnode vnode finished handoff and deleted.
2014-08-18 16:22:58.716 [debug] <0.597.0>@riak_core_vnode:finish_handoff:655 vnode hn/fwd :: riak_kv_vnode/182687704666362864775460604089535377456991567872 :: undefined -> 'dev2@127.0.0.1'
2014-08-18 16:22:58.717 [debug] <0.597.0>@riak_core_vnode:active:533 182687704666362864775460604089535377456991567872 riak_kv_vnode vnode excluded and unregistered.
...
```

### riak_core_vnode:finish_handoff

Handoffが完了したvnodeをunregisterしている。

```erlang
        Res when Res == forward; Res == shutdown ->
            {_, HN} = Target,
            %% Have to issue the delete now.  Once unregistered the
            %% vnode master will spin up a new vnode on demand.
            %% Shutdown the async pool beforehand, don't want callbacks
            %% running on non-existant data.
            maybe_shutdown_pool(State),
            {ok, NewModState} = Mod:delete(ModState),
            lager:debug("~p ~p vnode finished handoff and deleted.",
                        [Idx, Mod]),
            riak_core_vnode_manager:unregister_vnode(Idx, Mod),
            lager:debug("vnode hn/fwd :: ~p/~p :: ~p -> ~p~n",
                        [State#state.mod, State#state.index, State#state.forward, HN]),
            State2 = mod_set_forwarding(HN, State),
            continue(State2#state{modstate={deleted,NewModState}, % like to fail if used
                                  handoff_target=none,
                                  handoff_type=undefined,
                                  forward=HN})
```


### まとめ

通常のput/get(今回は見てないけど多分getもそう)時には、fallbackノードだからといって、
特殊な処理が入っているわけではない。
Preflist取得時に、死んでるprimaryノードがある場合、fallbackノードも対象に含めるだけ。

それとは別に、Riakノードは自分の持ってるパーティションが、自分ものか他ノードのものかを理解している。
joinやresizeなど、ownership handoff が完了した段階で、自分の手を離れた vnode をリストから除外する。

あとは riak_core_vnode_manager が他ノードの vnode の死活を検知した時に、
hinted handoff に関連する transfer が発生する（はず。まだそこまで読んでない）
