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

or

```
> make stagedevrel
...
> for i in `seq 1 5`; do sed -e 's/## ring_size = 64/ring_size=8/' -i.back dev/dev$i/etc/riak.conf ; done
> for i in `seq 1 5`; do sed -e 's/## strong_consistency = off/strong_consistency = on/' -i.back dev/dev$i/etc/riak.conf ; done
```

- per bucket type, `consistent = true`

```
$ riak-admin bucket-type create sc '{"props":{"consistent": true}}'
sc created
$ riak-admin bucket-type activate sc
$ riak attach
1> riak_ensemble_manager:enabled().
false
2> riak_ensemble_manager:enable().
ok
3> riak_core_ring_manager:force_update().
ok
```

- あとは普通のPUT/GET ... って思うでしょ？ 中でのコンセンサスが立ち上がるまでは `{error, <<"timeout">>}` が返ってくる
- 辛抱強く待つ => [see riak_test](https://github.com/basho/riak_test/blob/master/tests/ensemble_util.erl#L26)
- 30分くらいかかる？

```erlang
> riak_kv_ensembles:ensembles().
[{kv,0,3},
 {kv,22835963083295358096932575511191922182123945984,3},
 {kv,45671926166590716193865151022383844364247891968,3},
 {kv,68507889249886074290797726533575766546371837952,3},
 ....
 ]
> riak_ensemble_manager:cluster().
['dev1@127.0.0.1','dev2@127.0.0.1','dev3@127.0.0.1',
 'dev4@127.0.0.1','dev5@127.0.0.1']
> riak_kv_ensembles:check_membership().
[true,true,true,true,true,true,true,true,true,true,true,
 true,true,true,true,true,true,true,true,true,true,true,true,
 true,true,true,true,true,true|...]
> length(lists:filter(fun(E) -> riak_ensemble_manager:check_quorum(E, 500) end, riak_kv_ensembles:ensembles())).
20
> [ riak_ensemble_manager:count_quorum(E, 500) || E <- riak_kv_ensembles:ensembles()].
[0,0,0,0|...]
```
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


## Q

- membership change / ring update のときの挙動
- peer is fsm ; プロセスがどれくらいブロックするか？ kv_put/get を
 - peer でやると、ブロックが長いので遅いのでは
 - workerでやるとするとconsistency が心配
- leader peerがwriteを投げた直後に死んだら？
 - 古いデータが残ってない場合にどうやって戻すか
 - peerは履歴を持っているか
