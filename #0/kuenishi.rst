Riak SCR #0
===========

- お集まりいただきありがとうございます
- `16/16 <http://connpass.com/event/1265/>`_
- #riakjp


オマエ、誰よ？
===================

- @kuenishi 上西です
- `Bashoジャパン <http://www.basho.co.jp>`_ です
- 前職ではC/C++でとある分散システムを開発していました
- 9月に転職しました
- 現職ではErlang/OTPでとある分散システムを開発しています


開催の趣旨というかゴール
=========================

- Riakの内部を理解して @kuenishi と @ksauzz が日本でちゃんと仕事できるようになる
- Riakの中で使われている技術はすごいのか？すごくないのか？？がよくわからない
- riak_coreはフレームワークなので、それを使って何か作ってもらいたい


緊急アンケート
====================

- Hadoopを知っている？
- Hadoopを使ったことがある？
- Dynamoを知っている？
- Dynamoの論文を読んだことある？
- Cassandraを知っている？
- Cassandraを使ったことがある？
- Erlang/OTPを知っている？
- Erlang/OTPを使ってソフトウェアを真剣に書いたことがある？


それなりにざっくりでもいいので予習しておいてほしいこと
=======================================================

- `Dynamo <http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html>`_
- `Erlang/OTP <http://learnyousomeerlang.com>`_
- HTTPとかRPCとか基本的なところ


あると嬉しかったり話が早い知識
===================================

- DHT/Chord,
- Replication, Quorum, Vector Clock
- Bloom filter
- LevelDBとかLSM-tree 
- REST API - mochiweb, webmachineなど
- Distributed Erlang


だいたいの構成
===================

- `riak/rebar.conf <https://github.com/basho/riak/blob/master/rebar.config#L11>`_
- `riak_kv/rebar.conf <https://github.com/basho/riak_kv/blob/master/rebar.config#L11>`_
- `riak_core/rebar.conf <https://github.com/basho/riak_core/blob/master/rebar.config#L6>`_
- スライドのスタック
- size?

::

  $ cd riak
  $ find . -name "*.erl" | xargs wc 
  $ find . -name "*.erl" | xargs grep -v "^%" | wc


自己紹介タイム
===================

- オマエ、誰よ？
- バックグラウンド
- Riakに何を求めているか？
- この会に何を求めているか？


Discussion
===================

- 毎週か隔週でやりたい
- 何曜日がいい？オフィスは木曜以外なら
- 依存ライブラリ、割とキリがない

::

  $ cd riak
  $ ls deps


緊急アンケートその２
=========================

 - Riakのどこ|何に興味がある？
 - テーマ決めて割り振った方がよい？
 - 発表形式？読み合わせ形式？
 - 字だけのPPT禁止とかどうよ？
 - とりあえず次回、誰が何を
 - Back to basics?



重要なアンケート：ピザについて
================================

:あったほうがよい: ? %
:ないほうがよい:  100-? %  

重要なアンケート：ビールについて
==================================

:あったほうがよい: ? %
:ないほうがよい: ? %
:発表者は飲んではいけない: 100 %

その他
=========

- 連絡は `risk-users-jp <http://lists.basho.com/mailman/listinfo/riak-users-jp_lists.basho.com>`_ とTwitterかな？
- 継続が大切
- 頑張らない
- 負けないこと投げ出さないこと逃げ出さないこと信じ抜くこと
- トップバッターは @ksauzz です