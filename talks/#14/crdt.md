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

[PN Counter のデモ](http://github.com/basho/riak_crdt_cookbok) を使う

```
$ git clone git://github.com/basho/riak_crdt_cookbook
$ cd riak_crdt_cookbook/counters
$ bash counters_demo_setup.sh
```

[node1](http://localhost:10018/buckets/crdt_cookbook/keys/counters_demo.html) と
[node2](http://localhost:10028/buckets/crdt_cookbook/keys/counters_demo.html) を
別のウィンドウで開く

別にインクリしてみる

```
1> rpc:multicall(code, add_patha, [\"/home/kuenishi/src/riak_crdt_cookbook/counters\"]).
2> Healer = counters_demo:part([], newcookie).
```

別にインクリしてみる

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

## コールフロー

かなり奥底の方まで組み込まれているようで、仲間で入らないとコードは追えない系


# Related?

- Dynamo
- Concurrent Revisions
- Boom / Bloom; monotinic consistency
- (なんか昔のデータベース)
