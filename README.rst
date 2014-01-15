Riak Source Code Reading @Tokyo
===============================

- `Connpass series <http://connpass.com/series/218/>`_

- 一回あたりの基本方針

 - 隔週、19時スタート、できれば火曜日
 - Open Questionsから発表できそうな問題を調べてきて発表
 - 特に資料とかは不要 (ホワイトボードあり)
 - 発表した人にはマグカップまたはビアグラスをプレゼント（したい）
 - ビールはひとり2本まで
 - 参加者が10人を超えると立ち見になります、あしからず

- 皆勤賞の人にはTシャツをプレゼント（したい）
- 何か発表したい人、運営に意見あるひとはPull Requestでどうぞ


Open Questions - 発表のためのネタ
-------------------------------------

ソースコードレベルでの解説が基本(なにか気になったら随時Pull Requestください)

- Yokozuna
- Bucket Types
- Custer Metadata
- cuttlefish
- Strong Consistency
- Security
- Mutator

- Ring Resize
- Pagenated 2i

- node_package, leveldb
- riak_core ring 管理
- riak_core vnode 管理
- riak_core, riak_pipe 再入門
- 永続化まわりをちゃんと下まで([bitcask](http://downloads.basho.com/papers/bitcask-intro.pdf), leveldb)
- 障害時の動き(handoff やっているときの R/W/PR/PW, covering op),
- handoff の中身 (どのデータがどの vnode に返すものか分かるのはなぜ?)
- claimant 自身を `riak-admin cluster down` するときにはどう整合性が保たれているのか

開催の趣旨というかゴール
------------------------------

- Riakを使ったアプリの設計のポイント、運用や設定のノウハウを知りたい
- 特に障害時の動作


基礎知識
------------

- `Dynamo <http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html>`_ `和訳 <https://gist.github.com/2657692>`_
- Erlang/OTP全般

 - `erlang-users.jp <http://erlang-users.jp>`_
 - `なぜErlangなのか <http://ymotongpoo.hatenablog.com/entry/20110322/1300776826>`_

- キーワード

 - DHT/Chord, Replication, Quorum, Vector Clock, Bloom filter
 - LevelDBとかLSM-tree

- Riakドキュメント `オリジナル (英語) <http://docs.basho.com/riak/latest/>`_ / `日本語訳 <http://docs.basho.co.jp/riak/latest/>`_

Logs (Part 2: 2013/11 -)
------------------------

#17 2014/01/29
==============

- Ownership Handoff @takabow
- Plumtree @r_rudi

#16 2014/01/15
==============

- Mutator @ksauzz
- なにか @kuenishi

#15 2013/12/11
==============

- `riak_core <http://connpass.com/event/4270/>`_ by @shino

#14 2013/11/25
==============

- CRDT by @kuenishi
- riak_core ?

#13 2013/11/13
==============

- Yokozuna 徹底解説 by @ksauzz
- Bucket Types と Cluster Metadata のさわり by @shino
