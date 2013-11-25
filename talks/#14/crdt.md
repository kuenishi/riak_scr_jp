# 分散データベースにおける新しい整合性モデルとRiakにおける実装

- 2013 / 11 / 28 WebDB Forum
- Basho 上西康太
- 30min

# Basho紹介

- 分散データベース？
- Riak知ってる人？
- Riakというデータベース作っています

# Riak紹介

- OSSの分散データベース
- HadoopとかHBaseとかMongoDBとかそんなのばっかり
- 可用性にフォーカスしてるデータベースはまあ、Cassie以外はない
- Consistent Hashingでよしなにやる
- 可用性、運用性、予測可能性、安定性、…
- GCの影響
- CAP定理: トランザクションがない世界、Partition TolerantでAvailableな世界

 - HBaseはPartition TolerantでConsistentな複製
 - Availabilityは低い - Regionサーバーが落ちているときの書き込み、Masterが交代中のノード追加

- Partition TolerantかつAvailableにする方法

 - Vector Clocks: 複数バージョンを持っておく→読むときに解決 Dynamoが採っている方法
 - オフラインなときもとりあえず書き込ませる
 - Gitわかるひと？　Gitの発想に近い→必要に応じてマージ

(ちなみにDynamoDBは違う)

# CRDT紹介

- Grantがあるらしいが、どこかとJointプロジェクト

- Vector Clocksおかげでデータは絶対になくならない
- 読むのはアプリケーション的にはたいへん
- どうせ似たような設計になる

 - integer -> たす or max
 - flag -> どっちかに倒す or 多数決？
 - 状態とライフサイクルがある -> Timestamp / version

- 絵を描いて説明するか・ソースを読んで実装を知る必要がある

 - vnode_id is Actor, PNCounter> [{Actor, Inc,Dec}]
 - orset> orddict

- ライブラリ化した人がたくさんいる

 - Ruby, Python
 - Haskell (ふたりくらい？

- サーバー側でやってしまおう

# Demonstration

まだマージされてないのでブランチを使う

```
$ git clone git://github.com/basho/riak -b develop
$ cd riak
$ ./rebar get-deps
$ cd deps/riak_kv
$ git checkout feature/sdc/crdt-http-api
$ cd ../..
$ make stagedevrel
$ cd dev
$ ulimit -n 4096
$ dev1/bin/riak start
$ dev2/bin/riak start
$ dev1/bin/riak-admin cluster join dev2@127.0.0.1
$ dev1/bin/riak-admin cluster plan
$ dev1/bin/riak-admin cluster commit
```

## countersの場合は特に準備いらなかったのでデモ

1. [PN Counter のデモ](http://github.com/basho/riak_crdt_cookbok) を使う

```
$ git clone git://github.com/basho/riak_crdt_cookbook
$ cd riak_crdt_cookbook/counters
$ bash counters_demo_setup.sh
```
1. [node1](http://localhost:10018/buckets/crdt_cookbook/keys/counters_demo.html) と [node2](http://localhost:10028/buckets/crdt_cookbook/keys/counters_demo.html) を別のウィンドウで開く

2. 別にインクリしてみる

3. Network分断を起こす

```
1> rpc:multicall(code, add_patha, [\"/home/kuenishi/src/riak_crdt_cookbook/counters\"]).
2> Healer = counters_demo:part([], newcookie).
```

4. 分断した状況で別々にインクリしてみる

5. 分断を戻す

```
3> Healer().
```

See what happens!!


## setsの場合は bucket types を設定する必要がある

− sets はちょっといじればデモできる？？
```
$ riak-admin types ...
```

## Use cases

- Advert clicks (G-Counter)
- Shopping cart (Modified OR-Set)
- Logged in users (P-N Counter)
- Maps can be composed of complex data


# Code Reading

- 入り口API `riak_kv_web.erl` `riak_kv_wm_crdt.erl`
- `riak_dt` - データ型が入っているだけ
- `riak_kv` - 処理が書いてある

## データのフォーマット, update/merge

ふつうにGETすると壊れてみる

- update
- merge

これでなんとなく動きとデータ構造がわかる

### riak_dt

- `riak_dt.erl` behaviour

```erlang
-callback value(term(), crdt()) -> value().
-callback update(operation(), actor(), crdt()) -> {ok, crdt()} | {error, error()}.
-callback merge(crdt(), crdt()) -> crdt().
(...)
-callback to_binary(crdt()) -> binary().
-callback from_binary(binary()) -> crdt().
```

- `riak_dt_pncounter.erl` => `[{Actor,Increments,Decrements}]` ( increment, decrement )

```erlang
-opaque pncounter()  :: [{Actor::riak_dt:actor(), Inc::pos_integer(), Dec::pos_integer()}].
```

- `riak_dt_orset.erl` => `orddict::orddict()`
- `riak_dt_orswot.erl` => `{riak_dt_vclock:vclock(), [{member(), [{actor(), Count}]}]}`
- `riak_dt_map.erl` =>

```erlang
{riak_dt_vclock:vclock(),
    [{ {Name::term(), Type::crdt_mod()},
       {[{actor(),Count}], crdt()}  }]}.
```


## いろいろ試してみる

かなり奥底の方まで組み込まれているようで、仲間で入らないとコードは追えない系

HTTPは [riak_kv/#732](https://github.com/basho/riak_kv/pull/732) のブランチじゃないとまだ動かない。

`riak_kv_web.erl`

```erlang
     {["types", bucket_type, "buckets", bucket, "datatypes"], fun is_post/1,
      riak_kv_wm_crdt, [{api_version, 3}]},
     {["types", bucket_type, "buckets", bucket, "datatypes", key],
      riak_kv_wm_crdt, [{api_version, 3}]}] ++

        [ %% v1.4 counters @TODO REMOVE at v2.2
          %% NOTE: no (default) bucket prefix only
          {["buckets", bucket, "counters", key],
           riak_kv_wm_counter,
           APIv2Props}
        ] ++
```

## sets

bucket type から CRDT を叩いてみる

```sh
$ riak-admin bucket-type list
$ riak-admin bucket-type create stests '{"props":{"datatype":"set"}}'
stests created
$ riak-admin bucket-type activate stests
stests has been activated
$ riak-admin bucket-type list
stests (active)
$ curl http://localhost:10018/types/stests/props
{"props":{"young_vclock":20,"w":"quorum","small_vclock":50,"rw":"quorum","r":"quorum","pw":0,"precommit":[],"pr":0,"postcommit":[],"old_vclock":86400,"notfound_ok":true,"n_val":3,"linkfun":{"mod":"riak_kv_wm_link_walker","fun":"mapreduce_linkfun"},"last_write_wins":false,"dw":"quorum","chash_keyfun":{"mod":"riak_core_util","fun":"chash_std_keyfun"},"big_vclock":50,"basic_quorum":false,"allow_mult":true,"datatype":"set","active":true,"claimant":"dev1@127.0.0.1"}}
$ curl -X POST http://localhost:10018/types/stests/buckets/s/datatypes/k -H 'content-type: application/json' -d '{"add":"foobar"}'
$ curl http://localhost:10018/types/stests/buckets/s/datatypes/k
{"type":"set","value":["foobar"],"context":"SwEIGINsAAAAAWgCbQAAAAjFUrELUpIqH2EBagAAAAcBZm9vYmFyDYNsAAAAAWgCYQFhAWo="}
$ curl -X POST http://localhost:10018/types/stests/buckets/s/datatypes/k -H 'content-type: application/json' -d '{"remove":"foobar"}'
```

## Counters

```sh
$ riak-admin bucket-type create ctests '{"props":{"datatype":"counter"}}'
stests created
$ riak-admin bucket-type activate ctests
stests has been activated
$ riak-admin bucket-type list
stests (active)
ctests (active)
$ curl -X POST http://localhost:10018/types/ctests/buckets/c/datatypes/k -H 'content-type: application/json' -d '3'
$ curl -X POST http://localhost:10018/types/ctests/buckets/c/datatypes/k -H 'content-type: application/json' -d '"increment"'
$ curl -X POST http://localhost:10018/types/ctests/buckets/c/datatypes/k -H 'content-type: application/json' -d '{"decrement":3}'
$ curl http://localhost:10018/types/ctests/buckets/c/datatypes/k
{"type":"counter","value":1}
```
PUTとDELETEはできない

どんなHTTPなんだろう… -> `riak_kv_wm_crdt.erl` -> `riak_kv_crdt_json:update_request_from_json/3`

## maps

```sh
$ riak-admin bucket-type create mtests '{"props":{"datatype":"map"}}'
mtests created
$ riak-admin bucket-type activate mtests
mtests has been activated
$ riak-admin bucket-type list
stests (active)
ctests (active)
mtests (active)
$ curl -X POST http://localhost:10018/types/mtests/buckets/m/datatypes/k -H 'content-type: application/json' \
 -d '{"add": "foobar_set"}'
 $ curl -X POST http://localhost:10018/types/mtests/buckets/m/datatypes/k -H 'content-type: application/json' \
  -d '{"add": "foobar2_register"}'
```
他にも counter, flag, register, set, map が登録できる。 maps of maps は理屈ではできるが
```erlang
%%   The format of a field name in the map value determines both the
%%   name of the entry and the type, joined with an underscore. For
%%   example, a `register' with name `firstname' would be
%%   `"firstname_register"'. Valid types embeddable in a map are
%%   `counter', `flag', `register', `set', and `map'.
```

```sh
$ curl -X POST http://localhost:10018/types/mtests/buckets/m/datatypes/k -H 'content-type: application/json' \
  -d '{"update":{"foobar2_register":"boom!"}}'
$ curl -X POST http://localhost:10018/types/mtests/buckets/m/datatypes/k -H 'content-type: application/json' \
  -d '{"update":{"foobar2_set":"boom!", "foobar_counter":234}}'
$ curl http://localhost:10018/types/mtests/buckets/m/datatypes/k
```

## context とはいったい？

## コールフロー

`riak_kv_wm_crdt.erl` ではふつうにPUTしているだけ

```erlang
            {RD, Ctx} = maybe_generate_key(RD0, Ctx0),
            O = riak_kv_crdt:new({T, B}, Ctx#ctx.key, Mod),
            Options0 = make_options(Ctx),
            CrdtOp = make_operation(Mod, Op, OpCtx),
            Options = [{crdt_op, CrdtOp},
                       {retry_put_coordinator_failure,false}|Options0],
            case C:put(O, Options) of
```
Options に crdt_op を追加しているのが違うっぽい？

`riak_kv_vnode.erl` line 1161

```erlang
                    case handle_crdt(Coord, CRDTOp, VId, ObjToStore) of
                        {error, E} ->
                            {{fail, Idx, E}, PutArgs};
                        ObjToStore2 ->
                            {{true, ObjToStore2},
                             PutArgs#putargs{index_specs=IndexSpecs,
                                             is_index=IndexBackend}}
```

`riak_kv_vnode:handle_crdt/4` ここで組み込んでいる=> diverseしていたらmerge
diverseしていなかったら単にupdate

# Related?

- Dynamo
- Concurrent Revisions
- Boom / Bloom; monotinic consistency
- CRDT論文
- "CRDTs: Consistency without concurrency control", Mihai Letia, Nuno M. Preguiça, and Marc Shapiro. CoRR (2009)
- (なんか昔のデータベース)
