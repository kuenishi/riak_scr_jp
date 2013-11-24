!SLIDE

## Riak Source Code Reading #0

!SLIDE

### おまえだれよ

![pic](http://www.gravatar.com/avatar/3cb9536d180c2144c16db29a34e2fd39.png) @ksauzz

* Basho Japan
* ９月までWeb屋さん。Javaでバックエンドを細々と書いてました。

!SLIDE

### 内容

リクエストを受け付けてデータを書きにいくまでをざっくりと話します。

1. 処理フロー概要
1. Protocol Buffer interface
1. HTTP interface (webmachine)
1. その後続処理
    1. riak\_client
    1. riak\_kv\_put\_fsm
    1. 書き込み先ノードの選定付近

!SLIDE

## 処理フロー

!SLIDE

### 処理フロー

```
             |                                                    |
             |<---                riak_kv                    ---> |
             |                                                    |
 webmachine --- riak_wm_xxxx --+
          (HTTP)               |
                               +--- riak_client -- riak_kv_xxx_fsm
                               |                      (gen_fsm)
 riak_api   --- riak_pb_xxxx  -+
      (Protocol Buffer)
```
!SLIDE

## ソース概要: Protocol Buffer interface

!SLIDE

### ソース概要: Protocol Buffer interface

#### riak\_api -- サーバ部分

clientからの接続を待って、登録されたcallbackを呼び出す

```
▾ riak_api/
  ▾ src/
      riak_api.app.src
      riak_api_app.erl
      riak_api_basic_pb_service.erl
      riak_api_pb_listener.erl
      riak_api_pb_server.erl
      riak_api_pb_service.erl
      riak_api_pb_sup.erl
      riak_api_stat.erl
      riak_api_sup.erl
```

!SLIDE

### ソース概要: Protocol Buffer interface

#### riak\_kv\_pb\_xxxx 

protocol bufferの各データに応じたコールバック

```
▾ riak_kv/
  ▾ src/
    riak_kv_pb_bucket.erl
    riak_kv_pb_index.erl
    riak_kv_pb_mapred.erl
    riak_kv_pb_object.erl
```

!SLIDE

### ソース概要: Protocol Buffer interface

#### riak\_kv\_app:init/0

callbackの登録

```erlang
riak_api_pb_service:register(
        [{riak_kv_pb_object, 3, 6}, %% ClientID stuff
         {riak_kv_pb_object, 9, 14}, %% Object requests
         {riak_kv_pb_bucket, 15, 22}, %% Bucket requests
         {riak_kv_pb_mapred, 23, 24}, %% MapReduce requests
         {riak_kv_pb_index, 25, 26} %% Secondary index requests
         ]),

```

!SLIDE

### ソース概要: Protocol Buffer interface

IDはriak\_pb内の`riak_pb_codec.erl`に定義されている

```erlang
-spec msg_type(integer()) -> atom().
msg_type(0) -> rpberrorresp;
msg_type(1) -> rpbpingreq;
msg_type(2) -> rpbpingresp;
msg_type(3) -> rpbgetclientidreq;
msg_type(4) -> rpbgetclientidresp;
msg_type(5) -> rpbsetclientidreq;
msg_type(6) -> rpbsetclientidresp;
msg_type(7) -> rpbgetserverinforeq;
```

!SLIDE

### ソース概要: HTTP interface

!SLIDE

### ソース概要: HTTP interface

[Webmachine](http://wiki.basho.com/Webmachine-Dispatching.html)

* mochiweb上に作られたWeb framework
* REST toolkit
* dispatcher tableを使ってcallback(resource)のルールを定義

![pic](slides/dispatch_rule.png)

!SLIDE

### ソース概要: HTTP interface

Webmachineのdispatcher table, resource(callback) モジュール

```
▾ riak_kv/
  ▾ src/
      riak_kv_web.erl                <-- dispatcher table
      riak_kv_wm_buckets.erl         <-- 以下、コールバック
      riak_kv_wm_index.erl
      riak_kv_wm_keylist.erl
      riak_kv_wm_link_walker.erl
      riak_kv_wm_mapred.erl
      riak_kv_wm_object.erl
      riak_kv_wm_ping.erl
      riak_kv_wm_props.erl
      riak_kv_wm_raw.hrl
      riak_kv_wm_stats.erl
      riak_kv_wm_utils.erl
```

!SLIDE

###  dispatcher table - riak\_kv\_web

```erlang
dispatch_table() ->
    ...もろもろ省略...

     {["buckets", bucket, "keys", key],
      riak_kv_wm_object, Props2},
      raw_dispatch(),
     ...
```

<br>

`PUT /buckets/bucket_name/keys/key_name` に対応

!SLIDE

#### 一旦、Protocol buffer, HTTP interfaceに関してはここまでですが、
####  このまま、riak\_client以降へ読み進めます。

!SLIDE

### 処理フロー(復習)

```
             |                                                    |
             |<---                riak_kv                    ---> |
             |                                                    |
 webmachine --- riak_wm_xxxx --+
          (HTTP)               |
                               +--- riak_client -- riak_kv_xxx_fsm
                               |                      (gen_fsm)
 riak_api   --- riak_pb_xxxx  -+
      (Protocol Buffer)
```
!SLIDE

### HTTPのcallback module

###### object store処理(`PUT /buckets/bucket/keys/key`)

!SLIDE

###### riak\_kv\_wm\_object:accept\_doc\_body/2

```erlang
accept_doc_body(RD, Ctx=#ctx{bucket=B, key=K,
                       client=C, links=L, index_fields=IF}) ->
...     %% riak_client (parameterized module)
    case C:put(Doc, [{w, Ctx#ctx.w}, {dw, Ctx#ctx.dw},
                     {pw, Ctx#ctx.pw}, {timeout, 60000} |
                Options]) of
        {error, Reason} ->
            handle_common_error(Reason, RD, Ctx);
        ok ->
            {true, RD, Ctx#ctx{doc={ok, Doc}}};
        %% Siblingありの場合multiple choisesを返す
        {ok, RObj} ->
            DocCtx = Ctx#ctx{doc={ok, RObj}},
            HasSiblings = (select_doc(DocCtx) == multiple_choices),
            send_returnbody(RD, DocCtx, HasSiblings)
    end.

```

!SLIDE

#### riak\_client:put

```erlant
put(RObj, Options) when is_list(Options) ->
    ...
    case ClientId of
        undefined ->
            %% 子プロセスにput処理を委譲
            riak_kv_put_fsm_sup:start_put_fsm(Node, [{raw, ReqId, Me}, RObj, Options]);
        _ ->
            UpdObj = riak_object:increment_vclock(RObj, ClientId),
            riak_kv_put_fsm_sup:start_put_fsm(Node, [{raw, ReqId, Me}, UpdObj, [asis|Options]])
    end,
    %% TODO: Investigate adding a monitor here and eliminating the timeout.
    Timeout = recv_timeout(Options),
    wait_for_reqid(ReqId, Timeout);
```

!SLIDE

#### riak\_kv\_put\_fsm\_sup:start\_put\_fsm

```erlang
start_put_fsm(Node, Args) ->
    supervisor:start_child({?MODULE, Node}, Args).

init([]) ->
    PutFsmSpec = {undefined,
               {riak_kv_put_fsm, start_link, []},
               temporary, 5000, worker, [riak_kv_put_fsm]},

    {ok, {{simple_one_for_one, 10, 10}, [PutFsmSpec]}}.
```

!SLIDE

#### riak\_kv\_put\_fsm:start\_link

```erlang
start_link(From, Object, PutOptions) ->
    gen_fsm:start_link(?MODULE, [From, Object, PutOptions], []).

init([From, RObj, Options]) ->
    BKey = {Bucket, Key} = {riak_object:bucket(RObj), riak_object:key(RObj)},
    .. 省略 ..
    %% timeoutが即座に発生、prepareへ遷移
    {ok, prepare, StateData, 0};
```

!SLIDE

#### riak\_kv\_put\_fsm:prepare/2 書き込み先選定

```erlang
%% @private
prepare(timeout, StateData0 = #state{from = From, robj = RObj,
                                     bkey = BKey,
                                     options = Options}) ->
    %% Ringの取得 record定義はriak_core_ringのchstate_v2(たぶん)
    {ok,Ring} = riak_core_ring_manager:get_my_ring(),
    %% bucketPropsを取得。名前が...
    BucketProps = riak_core_bucket:get_bucket(riak_object:bucket(RObj), Ring),
    %% hash生成
    DocIdx = riak_core_util:chash_key(BKey),
    %% n_val
    N = proplists:get_value(n_val,BucketProps),
    %% ????
    StatTracked = proplists:get_value(stat_tracked, BucketProps, false),
```

!SLIDE

#### riak\_kv\_put\_fsm:prepare/2 つづき

```erlang
    %% up状態のnode一覧取得
    UpNodes = riak_core_node_watcher:nodes(riak_kv),
    %% DocIdxを基にup状態のpreference listを取得
    Preflist2 = riak_core_apl:get_apl_ann(DocIdx, N, Ring, UpNodes),
    ↑書き込み先ノード一覧
```

!SLIDE

#### riak\_core\_apl:get\_apl\_ann/4

##### DocIdxを基にRingを並び替えて, 且つup状態のノードの一覧を取得 (primaries + fallback nodes)

```erlang
-spec get_apl_ann(binary(), n_val(), ring(), [node()]) -> preflist2().
get_apl_ann(DocIdx, N, Ring, UpNodes) ->
    UpNodes1 = ordsets:from_list(UpNodes),
    %% DoxIdxに基いてRingのNode listを並び替え
    Preflist = riak_core_ring:preflist(DocIdx, Ring),
    %% n_valでPrimariesを選択
    {Primaries, Fallbacks} = lists:split(N, Preflist),
    %% Primaryiesをup/downノードで振り分け
    {Up, Pangs} = check_up(Primaries, UpNodes1, [], []),
    %% Fallbacksからupノードを抽出
    Up ++ find_fallbacks(Pangs, Fallbacks, UpNodes1, []).
```

!SLIDE

#### riak\_kv\_put\_fsm:prepare/2

##### 参考までに

```erlang
(riak@127.0.0.1)1> riak_core_ring_manager:get_my_ring().
{ok,{chstate_v2,'riak@127.0.0.1',
{'riak@127.0.0.1',{65,63519959152}}],
{64,
 [{0,'riak@127.0.0.1'},
  {22835963083295358096932575511191922182123945984, 'riak@127.0.0.1'},
  {45671926166590716193865151022383844364247891968, 'riak@127.0.0.1'},
  {68507889249886074290797726533575766546371837952, 'riak@127.0.0.1'},
  {91343852333181432387730302044767688728495783936, 'riak@127.0.0.1'},
  {114179815416476790484662877555959610910619729920, 'riak@127.0.0.1'},
  {137015778499772148581595453067151533092743675904, 'riak@127.0.0.1'},
  {159851741583067506678528028578343455274867621888, 'riak@127.0.0.1'},
    ....
```

!SLIDE

#### riak\_core\_apl:get\_apl\_ann/4

DocIdxを`71343852333181432387730302044767688728495783936`だとすると

```erlang
{0,'riak@127.0.0.1'},
{22835963083295358096932575511191922182123945984, 'riak@127.0.0.1'},
{45671926166590716193865151022383844364247891968, 'riak@127.0.0.1'},
{68507889249886074290797726533575766546371837952, 'riak@127.0.0.1'},
{91343852333181432387730302044767688728495783936, 'riak@127.0.0.1'},
{114179815416476790484662877555959610910619729920, 'riak@127.0.0.1'},
{137015778499772148581595453067151533092743675904, 'riak@127.0.0.1'},
{159851741583067506678528028578343455274867621888, 'riak@127.0.0.1'},
```

!SLIDE

#### riak\_core\_apl:get\_apl\_ann/4

DocIdxで並び替えて

```erlang
{91343852333181432387730302044767688728495783936, 'riak@127.0.0.1'},
{114179815416476790484662877555959610910619729920, 'riak@127.0.0.1'},
{137015778499772148581595453067151533092743675904, 'riak@127.0.0.1'},
{159851741583067506678528028578343455274867621888, 'riak@127.0.0.1'},
....
{0,'riak@127.0.0.1'},
{22835963083295358096932575511191922182123945984, 'riak@127.0.0.1'},
{45671926166590716193865151022383844364247891968, 'riak@127.0.0.1'},
{68507889249886074290797726533575766546371837952, 'riak@127.0.0.1'},
```

!SLIDE

#### riak\_core\_apl:get\_apl\_ann/4

UPノードのみにする

```erlang
{91343852333181432387730302044767688728495783936, 'riak@127.0.0.1'},
{114179815416476790484662877555959610910619729920, 'riak@127.0.0.1'},
{159851741583067506678528028578343455274867621888, 'riak@127.0.0.1'},
....
{22835963083295358096932575511191922182123945984, 'riak@127.0.0.1'},
{68507889249886074290797726533575766546371837952, 'riak@127.0.0.1'},
```

!SLIDE

ということでDocIdxを基にRingを並び替えて、
up状態のノードへデータを書き込む雰囲気。

![pic](http://wiki.basho.com/attachments/riak-data-distribution.png)

今日はここまで

!SLIDE

### 全体の雰囲気はつかめたでしょうか？？

```
             |                                                    |
             |<---                riak_kv                    ---> |
             |                                                    |
 webmachine --- riak_wm_xxxx --+
          (HTTP)               |
                               +--- riak_client -- riak_kv_xxx_fsm
                               |                      (gen_fsm)
 riak_api   --- riak_pb_xxxx  -+
      (Protocol Buffer)
```

!SLIDE

ご清聴ありがとうございました

<(\_ \_)>

