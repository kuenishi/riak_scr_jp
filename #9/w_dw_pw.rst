================================
Riak write properties: w, dw, pw
================================

Riak Source Code Reading @Tokyo #8, #9

Please refer to #8/w_dw_pw.rst. Thank you.

redbugで実際にコールスタックを見てみた
======================================

- redbug

  - `JLOUIS Ramblings <http://jlouisramblings.blogspot.jp/2010/11/tracing-erlang-programs-for-fun-and.html>`_
  - `鈴木さんの解説 <http://ksauzz.cs-ap-e1.ycloud.jp/slides/observer-eper/slide.html#0>`_

redbugは、erlang shellからmodule, functionを指定してフックを設定し、
指定したmoduleやfunctionが呼ばれた時点でその返り値やコールスタックを出力するツール。

基本文法
--------

   ``redbug:start(TimeOut, MessageCount, MS)``

- TimeOut

  - タイムアウト値(ms)。この時間を過ぎるとredbugが停止する。

- MessageCount

  - 生成されるreportの数

- MS

  - {module, function, [return, stack]}
  - {module,function,[return,{'_', 42}]} と書くと、module:function(_, 42)にマッチするコールスタックのみ表示する。

実践
----

使ってみる。

riak起動済みとして、


::

  $ ps aux | grep beam
  
  $ ./dev2/bin/riak attach
  
  
  > redbug:start(10000, 10, {riak_kv_put_fsm, put, [return, stack]}).
  ok
  quitting: timeout
  
  > redbug:start(10000, 10, {riak_client, put, [return, stack]}).
  ok

別ターミナルから、putしてみる

::

  $ curl -X PUT -i 'http://localhost:10018/buckets/accounts/keys/alice' -H 'Content-Type: application/json' -d '{name: "alice", age: 24}'
 

::

  quitting: timeout
 
なんかうまくいかない。

::

  > redbug:start(10000, 10, {erlang, now, [return, stack]}).
  ok
  ~~~~~
  15:12:24 <timer_server> {erlang,now,0} -> {1367,302344,603121}
  
  15:12:24 <timer_server> {erlang,now,[]}
    "proc_lib:init_p_do_apply/3 "
    "gen_server:handle_msg/5 "
    "timer:handle_info/2 "
  ~~~~~
  quitting: msg_count
  > 

erlang:now()は引っ掛けられていることを確認したので、文法的な問題ではなさそう。
なぜ？？？
