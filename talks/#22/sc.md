# Strong Consistency

- [Theory and Concepts of SC](http://docs.basho.com/riak/2.0.0beta1/theory/concepts/strong-consistency/)
- [Bringing Consistency to Riak (Part 2)](https://speakerdeck.com/jtuple/ricon-west-2012-bringing-consistency-to-riak-part-2)
- [RICON 2012 video](http://vimeo.com/51973001)

## Background

- ここでのConsistencyはACIDのCとは違うレプリケーションの文脈
- 冗長化された複製のデータ（Riakでは個々のKV）が一致していることを表す
- これまでRiakではQuorumとVCを使ったEventual Consistencyのみだった

- XXX Consistencyとは、整合性のレベルをあらわす言葉で、技術ではない
- そのConsistencyといってもLinearizable, Serializable, Causal ... などいろいろある

 - [Causal Consistency](http://en.wikipedia.org/wiki/Causal_consistency)
 - [Sequencial Consistency](http://en.wikipedia.org/wiki/Sequential_consistency)
 - [Strict Consistency, Linearizability](http://en.wikipedia.org/wiki/Linearizability)
 - [Serializability](http://en.wikipedia.org/wiki/Serializability)

- [Wikipedia の定義](http://en.wikipedia.org/wiki/Strong_consistency)によると、Sequencial Consistencyに近い

 - "All accesses are seen by all parallel processes (or nodes, processors etc.) in the same order (sequentially)"

- Sequencial Consistency = [逐次一貫性](http://ja.wikipedia.org/wiki/%E9%80%90%E6%AC%A1%E4%B8%80%E8%B2%AB%E6%80%A7)
- Strict Consistency を実現するには、Atomic Broad Cast protocolのようなプロトコルを設計しないといけない

- Atomic Broadcase Protocol 各種

 - Paxos
 - ZAB
 - Raft


- "Conditional Single Key Atomic Operations"
- "Puts fail without Vector Clock if object exists"
- "Consensus Group per preflist(replica set)"
- ring size 64 = 64 ensemble groups
- consensus state: epoch, sequence, membership, leader
- kv state: epoch, sequence, key, value

- Riak的には、Bucket typesでEventual ConsistencyとStrong Consistencyを使い分けられるようになる
- これが本当のTunable Consistencyだと思います

## Version

```sh
$ git clone git://github.com/basho/riak
$ cd riak
$ git checkout riak-2.0.0beta1 -b beta1
$ git log | head -n 6
commit 1f4d6c0aff4d1f48f0f15a0eae2b3817d1676da2
Author: Jared Morrow <jared@basho.com>
Date:   Fri Apr 11 15:32:48 2014 -0600

    Roll riak 2.0.0 beta1
$ make stagedevrel
```

## Usage

- riak.conf

```
strong_consistency = on
```

- per bucket type, `consistent = true`

```
$ riak-admin bucket-type create s '{"props":{"consistent": true}}'
s created
$ riak-admin bucket-type activate s
$ riak attach
1> riak_ensemble_manager:enabled().
false
2> riak_ensemble_manager:enable().
ok
```

- あとは普通のPUT/GET ... って思うでしょ？ 中でのコンセンサスが立ち上がるまでは `{error, <<"timeout">>}` が返ってくる
- 辛抱強く待つ

## TODOs

- AAE, 2i, stats > 2.0 final
- Datatypes, Multi-DC repl, lightweight Tx > Future
- benchmark soon


# Code

## riak_kv

- Bucket types (consistent, true)
- => `riak_kv_util:consistent_object/1`
-  => `lists:member({consistent, true}, riak_core_bucket:get_bucket(Bucket))`
- => consistent_(put|get)
-  => ensemble set: `{kv, Partition, N}`
-  => `riak_ensemble_client:kget/4` or `riak_ensemble_client:kupdate/6` (`ksafe_delete/5`)

## riak_ensemble

- `riak_ensemble_peer` is a fsm
-  => `riak_ensemble_peer:kget(..)`, `kupdate`, `kput_once`, `ksafe_delete`
- kover, kdelete
- `riak_ensemble_router:sync_send_event/5`

supervision tree

```
app
|
riak_ensemble_sup -(r41)-+- riak_ensemble_router_sup-(141)-riak_ensemble_router
                         +- riak_ensemble_peer_sup-(141)-riak_peer_router
                         +- riak_ensemble_manager
```

`r41`: `rest_for_one`
