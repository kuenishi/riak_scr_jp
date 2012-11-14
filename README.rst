Riak Source Code Reading @Tokyo
===============================

- `Connpass series <http://connpass.com/series/218/>`_

- 一回あたりの基本方針

 - 隔週、19時スタート、できれば火曜日
 - 入門・中級レベルと、ガチのソースコード解説を1セッションずつ
 - ビールはひとり2本まで
 - 発表した人にはマグカップをプレゼント（したい）

- 皆勤賞の人にはTシャツをプレゼント（したい）
- 何か発表したい人、運営に意見あるひとはPull Requestでどうぞ

Open Questions - 発表のためのネタ
-------------------------------------

ソースコードレベルでの解説が基本

- 障害時の挙動、primaryへの昇格は具体的にどうなるか？
- w, dw, pw ってなに？
- hinted_handoffってなに？
- 起動シーケンス
- テスト
- eper, etop, dtrace, tool群
- get, put 性能のオーダーとか勘所
- それぞれのAPIのレビュー
- インデックスがアトミックに更新されている？
- 遅いなと思ったときの解析とかチューニング (reserved for @itawasa)
- riak_pipe - MapReduceの処理フローとか
- sext
- folsom
- lager
- riak_api / erlang_protobuffs, mochiweb, poolboy, erlydtl, webmachineなど


#1 2012/11/28
-------------

- `Connpass page <http://connpass.com/event/1396/>`_
- Riak introduction by @IanMLewis
- Bitcask / LevelDB 使いわけ＆チューニング @csakatoku

#0 2012/11/13
-------------

- `Connpass page <http://connpass.com/event/1265/>`_
- `Keynote <https://gist.github.com/4044699>`_ by @kuenishi
-  `walkthrough around put request <http://gist-slide.appspot.com/4069613/slides.md>`_ by @ksauzz
