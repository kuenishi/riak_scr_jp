================================
Riak write properties: w, dw, pw
================================

Riak Source Code Reading @Tokyo #8, #9

Please refer to #8/w_dw_pw.rst. Thank you.

redbugで実際にコールスタックを見てみた
======================================

- redbug

  - `Official <https://code.google.com/p/eper/wiki/redbug>`_
  - `JLOUIS Ramblings <http://jlouisramblings.blogspot.jp/2010/11/tracing-erlang-programs-for-fun-and.html>`_
  - `鈴木さんの解説 <http://ksauzz.cs-ap-e1.ycloud.jp/slides/observer-eper/slide.html#0>`_

redbugは、erlang shellからmodule, functionを指定してフックを設定し、
指定したmoduleやfunctionが呼ばれた時点でその返り値やコールスタックを出力するツール。

基本文法
--------

  ``redbug:start(Trc,Opts).``

- Trc

  - sendかreceiveかRTP(String)のリストを取るとある。
  - RTPは, "<mfa> when <guards> -> <actions>"
  
    - mfaは、module, function, arity. mod:fun/arityで表記。'_'でワイルドカード。
    - guardsは、X==1とか
    - actionはreturn and/or stack.

      - returnだと、そのmod:func/arityの返り値を出力
      - stackだと、そのmod:func/arityが呼ばれるまでのスタックを出力

- Opts

  - 主要なものは以下
  - time
    
    - この時間(ms)でredbugによるトレースが打ち切られる
  
  - msgs
  
    - このメッセージ数に到達すると、トレースが打ち切られる
  
  - max_msg_size
    
    - stackの出力時に、quitting: {stack_size, xxx}と出るときは、この値を上げる必要がある。

  - print_depth

    - タプルやリストの出力時にどの階層まで出力するかを決める

(注）
以下のフォーマットではOptionの指定がうまくいかなかった。

  ``redbug:start(TimeOut, MessageCount, MS)``

- 例：redbug:start(10000, 10, {riak_client, put, [return, stack, {max_msg_size, 1000000}, {print_depth, 10}]}).
- おそらく、{_,_}をアリティに対するガードとして認識してしまっている。


実践
----

使ってみる。

riak起動済みとして、

::

  $ ./dev1/bin/riak attach  %% 最初dev2にattachしてうまくフック出来ず。鈴木さんに感謝。
  
  > redbug:start("riak_client:put -> stack;return", [{max_msg_size, 10000000}, {print_depth, 10}, {msgs, 100}, {time, 100000}]).
  ok

  ok

別ターミナルから、putしてみる

::

  $ curl -X PUT -i 'http://localhost:10018/buckets/accounts/keys/alice' -H 'Content-Type: application/json' -d '{name: "alice", age: 24}'
 

riak_client:put ::

  18:53:36 <{mochiweb_acceptor,init,3}> {riak_client,put,
                                         [{r_object,<<"accounts">>,<<"alica">>,
                                           [{...}],
                                           [],...},
                                          [{w,default},
                                           {dw,default},
                                           {pw,...},
                                           {...}],
                                          {riak_client,'dev1@127.0.0.1',
                                           undefined}]}
    "proc_lib:init_p_do_apply/3 "
    "mochiweb_http:parse_headers/5 "
    "webmachine_mochiweb:loop/1 "
    "webmachine_decision_core:handle_request/2 "
    "webmachine_decision_core:decision/1 "
    "webmachine_decision_core:accept_helper/0 "
    "webmachine_decision_core:resource_call/1 "
    "webmachine_resource:do/3 "
    "webmachine_resource:resource_call/3 "
  
  18:53:36 <{mochiweb_acceptor,init,3}> {riak_client,put,3} -> ok


ｷ･ｷ･ｷ･ｷﾀ━━━━━━(ﾟ∀ﾟ)━━━━━━!!!!

riak_clientがmochiweb_acceptorから呼ばれている様が分かります。

予想ではriak_wm_object:accept_doc_bodyから呼ばれてるはずだったのだが。
以下、主要なstack/returnを記載しておく。

riak_kv_put_fsm:start_link ::

  19:01:25 <{mochiweb_acceptor,init,3}> {riak_kv_put_fsm,start_link,
                                         [{raw,101500613,<0.10984.0>},
                                          {r_object,<<"accounts">>,<<"alic"...>>,
                                           [...],...},
                                          [{w,default},{dw,...},{...}|...]]}
    "proc_lib:init_p_do_apply/3 "
    "mochiweb_http:parse_headers/5 "
    "webmachine_mochiweb:loop/1 "
    "webmachine_decision_core:handle_request/2 "
    "webmachine_decision_core:decision/1 "
    "webmachine_decision_core:accept_helper/0 "
    "webmachine_decision_core:resource_call/1 "
    "webmachine_resource:do/3 "
    "webmachine_resource:resource_call/3 "
    "riak_kv_wm_object:accept_doc_body/2 "
  
  19:01:25 <{mochiweb_acceptor,init,3}> {riak_kv_put_fsm,start_link,3} -> {ok,
                                                                           <0.13955.0>}

riak_kv_put_fsm:validate ::

  19:09:51 <{riak_kv_put_fsm,init,1}> {riak_kv_put_fsm,validate,
                                       [timeout,
                                        {state,
                                         {raw,107633100,...},
                                         {r_object,...},
                                         [...],...}]}
    "proc_lib:init_p_do_apply/3 "
  
  19:09:51 <{riak_kv_put_fsm,init,1}> {riak_kv_put_fsm,validate,2} -> {next_state,
                                                                       precommit,
                                                                       {state,
                                                                        {raw,
                                                                         107633100,
                                                                         <0.9812.0>},
                                                                        {r_object,
                                                                         <<"acco"...>>,
                                                                         <<...>>,...},
                                                                        [{w,...},
                                                                         {...}|...],
                                                                        3,2,...},
                                                                       0}

riak_kv_put_fsm:execute ::

  19:12:00 <{riak_kv_put_fsm,init,1}> {riak_kv_put_fsm,execute,
                                       [{state,
                                         {raw,121563643,<0.8990.0>},
                                         {r_object,<<...>>,...},
                                         [{...}|...],
                                         3,...}]}
    "proc_lib:init_p_do_apply/3 "
  
  19:12:00 <{riak_kv_put_fsm,init,1}> {riak_kv_put_fsm,execute,1} -> {next_state,
                                                                      waiting_local_vnode,
                                                                      {state,
                                                                       {raw,
                                                                        121563643,
                                                                        <0.8990.0>},
                                                                       {r_object,
                                                                        <<"acco"...>>,
                                                                        <<...>>,...},
                                                                       [{w,...},
                                                                        {...}|...],
                                                                       3,2,...}}


riak_kv_vnode:put ::

  19:18:25 <{riak_kv_put_fsm,init,1}> {riak_kv_vnode,put,
                                       [[{479555224749202520035584085735030365824602865664,
                                          'dev2@127.0.0.1'},
                                         {502391187832497878132516661246222288006726811648,
                                          'dev3@127.0.0.1'}],
                                        {<<"accounts">>,<<"alica">>},
                                        {r_object,<<"acco"...>>,<<...>>,...},
                                        102168492,63534536305,[]|...]}
    "proc_lib:init_p_do_apply/3 "
    "gen_fsm:handle_msg/7 "
    "riak_kv_put_fsm:execute_remote/1 "
    "riak_kv_vnode:put/6 "
  
  19:18:25 <{riak_kv_put_fsm,init,1}> {riak_kv_vnode,put,6} -> ok

riak_kv_vnode:coord_put ::

  19:22:26 <{riak_kv_put_fsm,init,1}> {riak_kv_vnode,coord_put,
                                       [{456719261665907161938651510223838443642478919680,
                                         'dev1@127.0.0.1'},
                                        {<<"accounts">>,<<"alica">>},
                                        {r_object,<<"acco"...>>,<<...>>,...},
                                        123016420,63534536546,[]|...]}
    "proc_lib:init_p_do_apply/3 "
    "gen_fsm:handle_msg/7 "
    "riak_kv_put_fsm:execute_local/1 "
    "riak_kv_vnode:coord_put/6 "
  
  19:22:26 <{riak_kv_put_fsm,init,1}> {riak_kv_vnode,coord_put,6} -> ok

