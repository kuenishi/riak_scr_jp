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
- cuttlefish
- Security

- Ring Resize
- Pagenated 2i

- node_package, leveldb
- riak_core ring / vnode 管理
- riak_core, riak_pipe 再入門
- 永続化まわりをちゃんと下まで([bitcask](http://downloads.basho.com/papers/bitcask-intro.pdf), leveldb)
- 障害時の動き(handoff やっているときの R/W/PR/PW, covering op),
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

#25 2014/08/19
==============

- hinted handoff の中身 (どのデータがどの vnode に返すものか分かるのはなぜ?) by @takabow

#24 2014/07/23
==============

- Strong Consistency (cont) by @kuenishi => talks/#24/sc-2.md
- `当日の様子 <https://www.flickr.com/photos/38134009@N00/>`_

#23 2014/05/27
==============

- Security by @ksauzz

#22 2014/05/15
==============

- SC or Ring resize by @kuenishi => talks/#22/sc.md

#21 2014/04/21
==============

- Something about Softlayer by @maho_da
- Something about riak_core by @itawasa

#20 2014/04/02
==============

- Something about MapReduce by @ksauzz
- Something by Someone, maybe

#19 2014/03/18
==============

- dbg, redbug and more by @slfritchie
  - https://gist.github.com/slfritchie/9616500
- riak_core ring 2/N, raw ring and tainted ring by @shino

#18 2014/03/03
==============

- Handoff part 2 by @takabow

#17 2014/01/29 => 2/18
======================

- Ownership Handoff @takabow
- Plumtree @r_rudi

  `Riak 2.0のPlumtreeを読む <http://tdoc.info/blog/2014/01/10/riak_plumtree.html>`_
  `riakのhandoffについて調べたこと <http://tdoc.info/blog/2014/02/20/riak_handoff.html>`_

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
