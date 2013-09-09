Riak Source Code Reading @Tokyo
===============================

- `Connpass series <http://connpass.com/series/218/>`_

- 一回あたりの基本方針

 - 隔週、19時スタート、できれば火曜日
 - 入門・中級レベルと、ガチのソースコード解説を1セッションずつ
 - Open Questionsから発表できそうな問題を調べてきて発表
 - 特に資料とかは不要 (ホワイトボードあり)
 - 発表した人にはマグカップまたはビアグラスをプレゼント（したい）
 - ビールはひとり2本まで

- 皆勤賞の人にはTシャツをプレゼント（したい）
- 何か発表したい人、運営に意見あるひとはPull Requestでどうぞ


Open Questions - 発表のためのネタ
-------------------------------------

ソースコードレベルでの解説が基本(なにか気になったら随時Pull Requestください)

- 2.0新機能

 - Yokozuna, Bucket Types, Custer Metadata, CRDT, cuttlefish, Ring Resize
 - Pagenated 2i

- EQC pulse testing
- node_package
- riak_core, riak_pipe 再入門
- 永続化まわりをちゃんと下まで([bitcask](http://downloads.basho.com/papers/bitcask-intro.pdf), leveldb)
- 障害時の動き(handoff やっているときの R/W/PR/PW, covering op),
- handoff の中身 (どのデータがどの vnode に返すものか分かるのはなぜ?)

開催の趣旨というかゴール
------------------------------

- Riakを使ったアプリの設計のポイント、運用や設定のノウハウを知りたい
- 特に障害時の動作


基礎知識
------------

- `Dynamo <http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html>`_ `和訳 <https://gist.github.com/2657692>`_
- Erlang/OTP全般

 - `なぜErlangなのか <http://ymotongpoo.hatenablog.com/entry/20110322/1300776826>`_
 - `プログラミングErlang <http://www.amazon.co.jp/dp/4274067149>`_
 - `Learn You Some Erlang <http://learnyousomeerlang.com>`_
 - `Learn You Some Erlang 日本語訳 <http://www.ymotongpoo.com/works/lyse-ja/>`_ by ymotongpoo

- キーワード

 - DHT/Chord, Replication, Quorum, Vector Clock, Bloom filter
 - LevelDBとかLSM-tree

- Riakドキュメント `オリジナル (英語) <http://docs.basho.com/riak/latest/>`_ / `日本語訳 <http://docs.basho.co.jp/riak/latest/>`_

Logs
====

#13 Maybe September?
-------------------------

- Connpass page
- Yokozuna 入門 by @ksauzz
- QuickCheck でなにか @taketon_ (or riak_core by itawasa)

- first 2013/6/25 - but postponed
