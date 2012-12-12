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

- 障害時の挙動、primaryへの昇格は具体的にどうなるか？
- w, dw, pw ってなに？
- 起動シーケンス
- テスト
- eper, etop, dtrace, tool群
- get, put 性能のオーダーとか勘所
- deleteの動作（Tombstoneなど
- それぞれのAPIのレビュー
- インデックスがアトミックに更新されている？
- 遅いなと思ったときの解析とかチューニング (reserved for @itawasa)
- riak_pipe - MapReduceの処理フローとか
- sext
- folsom
- lager
- riak_api / erlang_protobuffs, mochiweb, poolboy, erlydtl, webmachineなど


開催の趣旨というかゴール
------------------------------

- Riakの内部を理解して @kuenishi と @ksauzz が日本でちゃんと仕事できるようになる
- Riakの中で使われている技術はすごいのか？すごくないのか？？がよくわからない
- riak_coreはフレームワークなので、それを使って何か作ってもらいたい
- Riakを使ったアプリの設計のポイント、運用や設定のノウハウを知りたい

基礎知識
------------

- `Dynamo <http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html>`_ `和訳 <https://gist.github.com/2657692>`_
- Erlang/OTP全般

 - `なぜErlangなのか <http://ymotongpoo.hatenablog.com/entry/20110322/1300776826>`_
 - `プログラミングErlang <http://www.amazon.co.jp/dp/4274067149>`_
 - `Learn You Some Erlang <http://learnyousomeerlang.com>`_
 - `Learn You Some Erlang 日本語訳 <http://www.ymotongpoo.com/works/lyse-ja/>`_ by ymotongpoo

- キーワード

 - DHT/Chord, Replication, Quorum, Vector Clock
 - Bloom filter
 - LevelDBとかLSM-tree
 - REST API - mochiweb, webmachineなど
 - Distributed Erlang

- Riakドキュメント `オリジナル (英語) <http://docs.basho.com/riak/latest/>`_ / `日本語訳 <http://docs.basho.co.jp/riak/latest/>`_

Logs
====

#3 2013/1/8
-----------

- `Connpass page <http://connpass.com/event/1383/>`_
- hinted_handoffってなに？ by @csakatoku
- Implementation of 2i by @kuenishi

#2 2012/12/11
-------------

- `Connpass page <http://connpass.com/event/1518/>`_
- `MapReduce <http://www.slideshare.net/masahitojp/riak-map-reduce-for-beginners-15608362>`_ by @Masahito
- Client使ってみた & Conflict Resolution (& riak_dt?) by @nobu_k

#1 2012/11/28
-------------

- `Connpass page <http://connpass.com/event/1396/>`_
- `Let's りあっくぅ <https://docs.google.com/presentation/d/1TEUie_V7kr6Z7reeNNnQTUQUWcWzFfHXFZxtgofEx5Q/edit#slide=id.p>`_ by @IanMLewis
- `Bitcask / LevelDB 使いわけ＆チューニング <https://github.com/kuenishi/riak_scr_jp/blob/master/%231/csakatoku.md>`_ @csakatoku
- Riakのインストール、Pythonクライアントで叩いてみた、riak_control
- bitcask / eleveldb の議論は割と盛り上がった

#0 2012/11/13
-------------

- `Connpass page <http://connpass.com/event/1265/>`_
- `Keynote <https://gist.github.com/4044699>`_ by @kuenishi
-  `walkthrough around put request <http://gist-slide.appspot.com/4069613/slides.md>`_ by @ksauzz

- 参加者：分散システムに興味あるひと、仕事に使いたい、MySQLであふれる、類似製品を扱っているetc - Erlangは初めてというひともちらほら
