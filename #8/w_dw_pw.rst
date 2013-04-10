================================
Riak write properties: w, dw, pw
================================

Riak Source Code Reading @Tokyo #8

:author: Shigekazu Takei ( github:takei-shg / Twitter: @taketon_ )
:date: 2013-04-09
:riak_core: ``2f13622`` Merge pull request #270 from basho/jrw-dialyzer-fixes
:riak_kv: ``aab11f4`` Update bitcask dependency to 1.6.1

.. contents:: :depth: 2

おまえ、誰よ
============
.. image:: https://ja.gravatar.com/userimage/39613705/84f5c119ac55ca18aa66344a760384e4.jpg

- `@taketon_ <https://twitter.com/taketon_>`_ 
- 業務パッケージ開発
- JavaとかJSとかHaskellとか
- Erlang : newbie
- ホップ++なビールが好き（ex: `インドの青鬼 <http://item.rakuten.co.jp/yonayona/854056/#854056>`_ ）

w, dw, pw?
==========

今日のお題
----------

- プロパティ(w, dw, pw)って何だ？

軽くおさらい
============

Riak
----
- CAP定理

  - AP重視型
  - Eventually Consistent, and **TUNABLE**

    - N:3, R:1, W:3  =  writeは遅く、readは早い。
    - N:3, R:3, W:1  =  writeは速く、readは遅い。
    - N:3, R:2, W:2  =  writeもreadも普通
    - どれも最新の値を読める

書き込み時
==========

.. image:: http://docs.basho.com/shared/1.3.0/images/riak-data-distribution.png?1364427191

- 最終的にはn_val個のレプリカつくる
- PUT要求時に成功とみなされるノードからの"書き込み成功"応答数 = **w**

書き込みはすぐには永続化されない
================================

- まずバッファに書き込み
- そのうち永続化される
- *すぐに永続化したい？*
- **dw**

  - `Durable write quorum`_
  - dwのノード数分、すぐにディスクへの書き込みを行う
  - default: 0

.. _`Durable write quorum`: http://docs.basho.com/riak/latest/references/Configuration-Files/

ノードダウン時の書き込み
========================

- 通常時の書き込み

  - 要求に対して `preference list`_ の上位W個のノードから応答する
  - preference listの上位W個に入っていたpartition(ノード？) = a primary

- ノードダウン時

  - preference listの残りからfallbackを選択
  - fallbackに対して、オブジェクトを元のpartitionIdと共に"一時的に"格納させる
  - fallbackは定期的に、 `Hinted Handoff`_ によってダウンしているノード(partition?)にデータを戻そうとする

- **pw**

  - `Primary write quorum`_
  - pwの数だけ、primaryノードに必ず書きこむ。でなければ失敗。
  - default: 0

.. _`preference list`: https://github.com/kuenishi/riak_scr_jp/blob/master/%230/ksauzz.md
.. _`Hinted Handoff`: https://github.com/kuenishi/riak_scr_jp/blob/master/%233/csakatoku.md
.. _`Primary write quorum`: http://docs.basho.com/riak/latest/references/Configuration-Files/

Let's read the Codes
====================

- riakがクライアントからのリクエストを受け取り、riak_kvの処理を呼び出すまで

  - https://github.com/takei-shg/riak_scr_jp/blob/master/%230/ksauzz.md


``riak_kv_wm_object:content_types_accepted/2``::

 content_types_accepted(RD, Ctx) ->
                       ~~~~~~~~~~
                     %% accept whatever the user says
                     {[{Media, accept_doc_body}], RD, Ctx};

``riak_kv_wm_object:accept_doc_body/2``::

 %% @spec accept_doc_body(reqdata(), context()) -> {true, reqdat(), context()}
 accept_doc_body(RD, Ctx=#ctx{bucket=B, key=K, client=C, links=L, index_fields=IF}) ->
     Doc0 = case Ctx#ctx.doc of
                {ok, D} -> D;
                _       -> riak_object:new(B, K, <<>>)
            end,
     VclockDoc = riak_object:set_vclock(Doc0, decode_vclock_header(RD)),
       ~~~~~~~~~~~
     Doc = riak_object:update_value(MDDoc, riak_kv_wm_utils:accept_value(CType, wrq:req_body(RD))),
     Options = case wrq:get_qs_value(?Q_RETURNBODY, RD) of ?Q_TRUE -> [returnbody]; _ -> [] end,
 
           %% calling riak_client:put/2
     case C:put(Doc, [{w, Ctx#ctx.w}, {dw, Ctx#ctx.dw}, {pw, Ctx#ctx.pw}, {timeout, 60000} |
                 Options]) of
         {error, Reason} ->
             handle_common_error(Reason, RD, Ctx);
         ok ->
             {true, RD, Ctx#ctx{doc={ok, Doc}}};
         {ok, RObj} ->
             DocCtx = Ctx#ctx{doc={ok, RObj}},
             HasSiblings = (select_doc(DocCtx) == multiple_choices),
             send_returnbody(RD, DocCtx, HasSiblings)
     end.

riak_client.erl
===============

``riak_client:put/2``::

 put(RObj, Options) when is_list(Options) ->
     Me = self(),
     ReqId = mk_reqid(),
     case ClientId of
         undefined ->
             case node() of
                 Node ->
                     riak_kv_put_fsm:start_link({raw, ReqId, Me}, RObj, Options);
                 _ ->
                     proc_lib:spawn_link(Node, riak_kv_put_fsm, start_link,
                                         [{raw, ReqId, Me}, RObj, Options])
             end;

- version 1.3.1から、riak_kv_put_fsm_sup経由でなく、直接riak_kv_put_fsm:start_link/3を呼ぶ形になっている。
- Nodeにはnode()により現在のノードの名前が格納されている。

``riak_kv_put_fsm:start_link/3``::

 start_link(From, Object, PutOptions) ->
     gen_fsm:start_link(?MODULE, [From, Object, PutOptions], []).

- gen_fsmによるstate管理開始
- Fromはnode()の値

gen_fsm:start_link
==================

- gen_fsm 
  
  - http://www.erlang.org/doc/man/gen_fsm.html
  - OTPにより提供されるbehaviour.
  - Generic Finite State Machine Behaviour
  - gen_fsmに対してModuleを引数で渡し、そのModuleの対応するcallbackを呼ばせる

    - gen_fsm:start_link(riak_kv_put_fsm, , ) -> riak_kv_put_fsm:init/1

  - callbackモジュール側にはfsmが持つ状態名のcallback関数を定義しておく

    - ex) riak_kv_put_fsm

      - riak_kv_put_fsm:prepare/2
      - riak_kv_put_fsm:validate/2
      - riak_kv_put_fsm:precommit/2
      - etc.

    - 各callback関数は{next_state, 遷移先fsm状態, StateData(gen_fsm自体のもつ状態)}を返し、次の状態へfsmを遷移させる 

  - gen_fsm:start_linkは同期的なので、initが完了してgen_fsmが初期化されるまでは値を返さない
  - gen_fsmはパフォーマンス的にはあまりよくないが状態管理がしやすいのでriakでよく使われる(by @kuenishiさん)

gen_fsm:start_link/3はcallbackのinit/1を実行、  
{ok, prepare, StateData, 0}を返し、初期状態をprepareとした形で初期化が完了する。

``riak_kv_put_fsm:init/1``::

 %% @private
 init([From, RObj, Options]) ->
     BKey = {Bucket, Key} = {riak_object:bucket(RObj), riak_object:key(RObj)},
     StateData = add_timing(prepare, #state{from = From,
                                            robj = RObj,
                                            bkey = BKey,
                                            options = Options}),
     riak_kv_get_put_monitor:put_fsm_spawned(self()),
     riak_core_dtrace:put_tag(io_lib:format("~p,~p", [Bucket, Key])),
       ~~~~~~~~
     ?DTRACE(?C_PUT_FSM_INIT, [TombNum], ["init", TombStr]),
     {ok, prepare, StateData, 0};

gen_fsm:prepare
===============

- gen_fsm:send_event/2がriak_kv_vnode/handle_command(?KV_PUT_REQ,,,)から呼ばれたときに、gen_fsmの遷移が開始する。（詳細は#9にて発表）
- initによりprepare状態になっていたはずなので、prepareから始める。

``riak_kv_put_fsm:prepare/2``::

 prepare(timeout, StateData0 = #state{from = From, robj = RObj,
                                      bkey = BKey,
                                      options = Options}) ->
       ~~~~~~~~~~~~~~
       %% preference listを確認
     case {Preflist2, LocalPL =:= [] andalso Must == true} of
           ~~~~~~~~~~~
         _ ->
               ~~~~~~~~~~~
             StateData = StateData0#state{n = N,
                                          bucket_props = BucketProps,
                                          coord_pl_entry = CoordPLEntry,
                                          preflist2 = Preflist2,
                                          starttime = StartTime,
                                          tracked_bucket = StatTracked},
             ?DTRACE(?C_PUT_FSM_PREPARE, [0], ["prepare", CoordPlNode]),
             new_state_timeout(validate, StateData)
     end.

- 書込み先が自ノードでなければ書込み先ノードに移り、initからやり直し。(該当コードは省略）
- 自ノードであればvalidateに進む

``riak_kv_put_fsm:new_state_timeout/2``::

 new_state_timeout(StateName, StateData) ->
     {next_state, StateName, add_timing(StateName, StateData), 0}.

- next_stateでvalidateを指定。timeoutが0なので、validateへ遷移

続きは#9で
==========

gen_fsm:validate
================

``riak_kv_put_fsm:validate/2``::

 validate(timeout, StateData0 = #state{from = {raw, ReqId, _Pid},
                                       options = Options0,
                                       n=N, bucket_props = BucketProps,
                                       preflist2 = Preflist2}) ->
        ~~~~~~~~~~
     if
           ~~~~~~~~~~~
         true ->
               ~~~~~~~~~~~~~
             StateData1 = StateData0#state{n=N,
                                           w=W,
                                           pw=PW, dw=DW, allowmult=AllowMult,
                                           precommit = Precommit,
                                           postcommit = Postcommit,
                                           req_id = ReqId,
                                           timeout = Timeout},
             Options = flatten_options(proplists:unfold(Options0 ++ ?DEFAULT_OPTS), []),
             StateData2 = handle_options(Options, StateData1),
             StateData3 = apply_updates(StateData2),
             StateData = init_putcore(StateData3, IdxType),
             ?DTRACE(?C_PUT_FSM_VALIDATE, [N, W, PW, DW], []),
             case Precommit of
                 [] -> % Nothing to run, spare the timing code
                     execute(StateData);
                 _ ->
                     new_state_timeout(precommit, StateData)
             end
     end.

``riak_kv_put_fsm:execute/1``::

 execute(State=#state{coord_pl_entry = CPL}) ->
     case CPL of
         undefined ->
             execute_remote(State);
         _ ->
             execute_local(State)
     end.

riak_kv_put_fsm:execute_local/1
===============================

``riak_kv_put_fsm:execute_local/1``::

 %% Send the put coordinating put requests to the local vnode - the returned object
 %% will guarantee a frontier object.
 %% N.B. Not actually a state - here in the source to make reading the flow easier
 execute_local(StateData=#state{robj=RObj, req_id = ReqId,
                                 timeout=Timeout, bkey=BKey,
                                 coord_pl_entry = {_Index, Node} = CoordPLEntry,
                                 vnode_options=VnodeOptions,
                                 starttime = StartTime}) ->
     ?DTRACE(?C_PUT_FSM_EXECUTE_LOCAL, [], [atom2list(Node)]),
     StateData1 = add_timing(execute_local, StateData),
     TRef = schedule_timeout(Timeout),
     riak_kv_vnode:coord_put(CoordPLEntry, BKey, RObj, ReqId, StartTime, VnodeOptions),
     StateData2 = StateData1#state{robj = RObj, tref = TRef},
     %% Must always wait for local vnode - it contains the object with updated vclock
     %% to use for the remotes. (Ignore optimization for N=1 case for now).
     new_state(waiting_local_vnode, StateData2).
 
riak_kv_vnode:coord_put/6
=========================

``riak_kv_vnode:coord_put/6``::

 %% Issue a put for the object to the preflist, expecting a reply
 %% to an FSM.
 coord_put(IndexNode, BKey, Obj, ReqId, StartTime, Options) when is_integer(StartTime) ->
     coord_put(IndexNode, BKey, Obj, ReqId, StartTime, Options, {fsm, undefined, self()}).
 
 coord_put(IndexNode, BKey, Obj, ReqId, StartTime, Options, Sender)
   when is_integer(StartTime) ->
     riak_core_vnode_master:command(IndexNode,
                                    ?KV_PUT_REQ{
                                       bkey = BKey,
                                       object = Obj,
                                       req_id = ReqId,
                                       start_time = StartTime,
                                       options = [coord | Options]},
                                    Sender,
                                    riak_kv_vnode_master).
 
``riak_core_vnode_master:command``::

 %% Send the command to the preflist given with responses going to Sender
 command([], _Msg, _Sender, _VMaster) ->
     ok;
 command([{Index, Pid}|Rest], Msg, Sender, VMaster) when is_pid(Pid) ->
     gen_fsm:send_event(Pid, make_request(Msg, Sender, Index)),
     command(Rest, Msg, Sender, VMaster);
 command([{Index,Node}|Rest], Msg, Sender, VMaster) ->
     proxy_cast({VMaster, Node}, make_request(Msg, Sender, Index)),
     command(Rest, Msg, Sender, VMaster);

参考
====

- [riak/docs/configuration files] http://docs.basho.com/riak/latest/references/Configuration-Files/
- [riak/docs/eventual consistency] http://docs.basho.com/riak/latest/references/appendices/concepts/Eventual-Consistency/
- [riak/docs/replication] http://docs.basho.com/riak/latest/references/appendices/concepts/Replication/
- [preference list] https://github.com/kuenishi/riak_scr_jp/blob/master/%230/ksauzz.md
- [Hinted Handoff] https://github.com/kuenishi/riak_scr_jp/blob/master/%233/csakatoku.md
- [gen_fsm] http://www.erlang.org/doc/design_principles/fsm.html
- [gen_fsm behaviour] http://www.erlang.org/doc/design_principles/fsm.html#id69044
- [gen_fsm tutorial] http://pdincau.wordpress.com/2010/09/07/an-introduction-to-gen_fsm-behaviour/
