# Strong Consistency

# review of #22

- Joe's presentation
- How it works
- Demonstration

# References / Documentations

- すごく簡単な使い方の説明: [Strong Consistency @ basho_docs](https://github.com/basho/basho_docs/blob/features/lp/strong-consistency/source/languages/en/riak/dev/advanced/strong-consistency.md)
- [Release Note](https://github.com/basho/riak_ee/blob/release-note-ja-2.0rc1/RELEASE-NOTES.ja.md#strong-consistency)
- もうちょっと詳しい使い方や仕組みの説明: [User Docs @ riak_ensemble](https://github.com/basho/riak_ensemble/blob/wip/riak-2.0-user-docs/riak_consistent_user_docs.md)
- くわしい設計の説明: [Design Doc @ riak_ensemble](https://github.com/basho/riak_ensemble/tree/feature/add-docs/doc)

# Questions

- Bootstrap sequence
- ノード追加はどう動くの？
- 1/3 書かれなかったデータはどうやって復旧するの？
- Partial Failure は大丈夫？


# Basic Concepts

## Leader Leases

- [Leader Leases](https://github.com/basho/riak_ensemble/blob/wip/riak-2.0-user-docs/riak_consistent_user_docs.md#leader-leases)

- レプリカセットのなかからリーダーを選ぶ
- リースが切れてリーダーがFollowerから捨てられるまでは次のリーダーは選べない
- 時計は [Erlangの時計](http://www.erlang.org/doc/apps/erts/time_correction.html) と OS のmonotinic clockの組み合わせでダブルチェックしている（ERTSの実装かな？
 - erts は `gettimeofday(2)` => [src](https://github.com/erlang/otp/blob/OTP-17.0/erts/emulator/sys/unix/erl_unix_sys.h#L162) なので戻る場合がある（それをソフトウェアで動かないことを保証している）
 - riak_ensemble に NIF が入っている : `riak_ensemble_clock:monotonic_time/0`

 - FollowerのタイムアウトはLeader leaseの4倍
 - `follower_timeout` : default `4*ensemble_tick`
 - `trust_lease` という設定を `false` にすると常に quorum read ができる

- `ensemble_tick` という設定値で変更（default: 500ms）
- `lease_duration`

## Data Corruption

- 基本的には 2重故障が起きるとタイヘンなことになる

 - Valid commit (ただし 2/3 commit) の直後に1台こわれると複製が 1個に縮退して二度と読めなくなる→検出できないデータ破壊
 - なので、本当に絶対にデータをなくしたくないなら N=5,7 がオススメ（create bucket-type時に設定、ただし `target_n_val` も7とか9に変更しないといけない）
 - => `etc/advanced.config`

- その他の大抵のケースは大丈夫

 - epoch X なsetに epoch Y (where X < Y) なデータがリストアされても大丈夫
 - レプリカセット内のデータの整合性は Merkle Tree を持っていて、それを write 毎に更新している？
 - 少なくとも read 毎にチェックしている
 - 再起動なんかで `untrusted` な状態になったら基本的には unavailable

- そういうわけで、再起動はわりと注意が必要

# Additions since last time

なんかいっぱいコマンドが増えている。最大8台からになっている（確か）

```sh
> git clone git://github.com/basho/riak -b 2.0
> cd riak
> make stagedevrel
> for i in dev/** ; do sed -e 's/## strong_consistency = on/strong_consistency = on/' -i.back $i/etc/riak.conf; done
> grep consistency dev/dev**/etc/riak.conf
dev/dev1/etc/riak.conf:strong_consistency = on
dev/dev2/etc/riak.conf:strong_consistency = on
dev/dev3/etc/riak.conf:strong_consistency = on
dev/dev4/etc/riak.conf:strong_consistency = on
dev/dev5/etc/riak.conf:strong_consistency = on
dev/dev6/etc/riak.conf:strong_consistency = on
dev/dev7/etc/riak.conf:strong_consistency = on
dev/dev8/etc/riak.conf:strong_consistency = on
> for i in dev/** ; do $i/bin/riak start
> for i in dev/** ; do $i/bin/riak ping ; done
pong
pong
pong
pong
pong
pong
pong
pong
> for i in dev/** ; do $i/bin/riak-admin cluster join dev1@127.0.0.1 ; done
Failed: This node cannot join itself in a cluster
Success: staged join request for 'dev2@127.0.0.1' to 'dev1@127.0.0.1'
Success: staged join request for 'dev3@127.0.0.1' to 'dev1@127.0.0.1'
Success: staged join request for 'dev4@127.0.0.1' to 'dev1@127.0.0.1'
Success: staged join request for 'dev5@127.0.0.1' to 'dev1@127.0.0.1'
Success: staged join request for 'dev6@127.0.0.1' to 'dev1@127.0.0.1'
Success: staged join request for 'dev7@127.0.0.1' to 'dev1@127.0.0.1'
Success: staged join request for 'dev8@127.0.0.1' to 'dev1@127.0.0.1'
> dev/dev1/bin/riak-admin plan
> dev/dev1/bin/riak-admin commit
> dev/dev1/bin/riak-admin transfer-limit 100
> dev/dev1/bin/riak-admin bucket-type create sc '{"props":{"strongly_consistent":true}}
> dev/dev1/bin/riak-admin bucket-type status sc
> dev/dev1/bin/riak-admin bucket-type activate sc
> dev/dev1/bin/riak-admin ensemble-status
> dev/dev1/bin/riak-admin ensemble-status root
> dev/dev1/bin/riak-admin ensemble-status 2
> curl -XPUT http://localhost:10018/types/sc/buckets/b/keys/k -d '1'
> curl -XPUT http://localhost:10018/types/sc/buckets/b/keys/k -d '2'
```

# Limitations

- tombstoneは回収されない
- 何もないところにreadを飛ばすとtombstoneが作られる
- Keylistingするとtombstoneが引っかかるｗ
- 2iは動かない
- Searchは動くけどIndexとKeyはConsistentじゃない
- クラスタ間レプリケーションはされない（もし設定されていたとしても）

# Source

2.0 branch:

```
commit 802816ee42ca0fdd1fb88ff90d19be0a7a07f5db
Refs: 2.0, <origin/2.0>, riak-2.0.0rc1-1-g802816e
Author:     Luc Perkins <lucperkins@gmail.com>
AuthorDate: Mon Jul 21 13:12:58 2014 -0700
Commit:     Luc Perkins <lucperkins@gmail.com>
CommitDate: Mon Jul 21 13:12:58 2014 -0700
```

今日はブートシーケンスのようなもの

- claimant tick 毎に `riak_core_claimant:maybe_enable_ensembles/0` を実行
- `riak_core_claimant:enable_ensembles/1`

 - `riak_core_ring:get_meta('$ensemble_singleton', Ring)`
 - 誰もいなければ自分が `$ensemble_singleton` になる => update ring

- （次の tick で）自分が singleton なら `riak_ensemble_manager:enable/0` がスタート
- `riak_ensemble_manager:activate/1`

 - `{root, node()} => #ensemble_info{...}`

```erlang
            RootInfo = {root, RootLeader, {Root#ensemble_info.vsn,
                                           Root#ensemble_info.views},
                        undefined},
```

- Activated State を `riak_ensemble_manager:maybe_save_state/1`

  - new state を `riak_ensemble_storage:put/2`

- `riak_ensemble_storage:sync/1`
- `ets:insert(?ETS, RootInfo)`
- `riak_ensemble_manager:state_changed/1`

 - いろいろ ets に保存
 - `cluster_state`, `cluster`, `enabled`
 - `check_ensembles/2` で差分をみつけて、etsを更新
 - `check_peers/1`
 - => Changesがあれば `riak_kv_ensembles:ready_to_start/0` をみて `riak_ensemble_peer_sup:start_peer/4` (`stop_peer`）する
 - => Peerがspawnされる / Peerがstopされる

## Riak KV での bootstrap

- `riak_ensemble_peer_sup:start_child` => `riak_ensemble_peer:init/1`

 - `x` とかいう名前の ets table を作っているｗ
 - ふつうの gen_fsm
 - Stateをつくる
 - `riak_ensemble_peer_sup:register_peer/4` => ETS(riak_ensemble_peers) に peer の pid, ets tid を登録
 - #state.mod => `riak_kv_ensemble`
 - 自分に init を送って非同期で起動（timeoutじゃないんや…）

- `riak_ensemble_peer:setup/2`

 - `open_hashtree`, `reload_fact`
 - `start_workers` : `riak_ensemble_peer_worker:start/1`
 - compute_members
 - `riak_ensemble_lease:start_link/0`


# Q&A

- あとから n_val=5 とかで consistent bucket-type をつくったらどうなる？ => 試してみたが、 ensemble-status ではよくわからなかった
