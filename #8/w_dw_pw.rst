================================
Riak write properties: w, dw, pw
================================

Riak Source Code Reading @Tokyo #8, #9

:author: Shigekazu Takei ( github:takei-shg / Twitter: @taketon_ )
:date: 2013-04-09, 2013-04-23
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

注（追記）
----------

redbugでコールスタックを取った様子は、#9/w_dw_pw.rstに記載しました。

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
- *すぐに永続化したい場合*
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

Let's read the Code
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
         _ ->
             UpdObj = riak_object:increment_vclock(RObj, ClientId),
             case node() of
                 Node ->
                     riak_kv_put_fsm:start_link({raw, ReqId, Me}, UpdObj, [asis|Options]);
                 _ ->
                     proc_lib:spawn_link(Node, riak_kv_put_fsm, start_link,
                                         [{raw, ReqId, Me}, RObj, [asis|Options]])
             end
     end,
     %% TODO: Investigate adding a monitor here and eliminating the timeout.
     Timeout = recv_timeout(Options),
     wait_for_reqid(ReqId, Timeout);

- version 1.3.1から、riak_kv_put_fsm_sup経由でなく、直接riak_kv_put_fsm:start_link/3を呼ぶ形になっている。
- Nodeにはnode()により現在のノードの名前が格納されている。
- riak_kv_put_fsm:start_link/3を呼んで、書き込みの管理をriak_kv_put_fsmに委譲

  - この関数内では、riak_kv_put_fsm:initからのメッセージを受け取った後、wait_for_reqidでメッセージを待ち続ける。
  - riak_kv_put_fsmでは、init -> prepare -> validate -> waiting_local_vnode -> waiting_remote_vnode -> finish -> stopと遷移する。
  - riak_kv_put_fsmで書き込みが終了すると、riak_client:wait_for_reqidの中のreceiveにメッセージが届く

.. image:: https://pbs.twimg.com/media/BIjMdN4CUAEeqYr.png

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

- gen_fsm:start_link/3はcallbackのinit/1を実行
- riak_kv_put_fsm:init/1は,{ok, prepare, StateData, 0}を返す
- Timeoutが0なので、そのままprepareが呼ばれる。

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

ここで、riak_kv_vnodeに関わるプロセスを整理
=============================================

- riak_core_vnode_master (behaviour = gen_server)

  - start_vnode/2でriak_core_vnode_manager:start_vnode/2を呼ぶ

    - 最終的に、riak_core_vnode_sup:start_vnode/3でriak_core_vnodeプロセスを起動
    - riak_core_vnodeでstarted(wait_for_init...)が呼ばれて、初期化完了。riak_core_vnodeはactive状態に。

- riak_core_vnode_manager (behaviour = gen_server)

  - vnodeを管理している。get_vnodeとかhandoffとか。

- riak_core_vnode (behaviour = gen_fsm)

  - 状態遷移：init -> started -> active ( -> stop )
  - active状態で、?VNODE_REQを受け、riak_kv_vnode:handle_commandをコール
  - riak_kv_vnodeはbehaviour = riak_core_vnodeとして動作する
    
gen_fsm:prepare
===============

- initの返り値にTimeout=0がセットされている。
- よって、Module:StateName/2でハンドルされるので、すぐに次のprepareに遷移。

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

- init_putcoreで、書き込み成功/失敗数を管理するPutcoreをStateに設定する

  - Putcoreが別モジュールに切り出されているのは、おそらくテスタビリティのため 

- Putcoreの内容はriak_kv_put_fsm:waiting_local_vnode等でUpdateされる。

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

- add_timingは、OSのtimestampを記録している。おそらくRiak独自の統計情報に使用する(書き込みに何秒かかったとか) 
- riak_kv_vnode:coord_put/6で書き込みを行う
- riak_kv_vnode自身はwaiting_local_vnode状態で書き込みの結果を待つ

``riak_kv_put_fsm:waiting_local_vnode/2``::

 waiting_local_vnode(Result, StateData = #state{putcore = PutCore}) ->

     %% putcoreの累積値をupdate
     UpdPutCore1 = riak_kv_put_core:add_result(Result, PutCore),
     case Result of
         {fail, Idx, _ReqId} ->
             ?DTRACE(?C_PUT_FSM_WAITING_LOCAL_VNODE, [-1],
                     [integer_to_list(Idx)]),
             %% Local vnode failure is enough to sink whole operation
             process_reply({error, local_put_failed}, StateData#state{putcore = UpdPutCore1});

         %% wが来ただけではlocalで書き込みが完了したか分からないので、まだwaiting_local_vnode
         {w, Idx, _ReqId} ->
             ?DTRACE(?C_PUT_FSM_WAITING_LOCAL_VNODE, [1],
                     [integer_to_list(Idx)]),
             {next_state, waiting_local_vnode, StateData#state{putcore = UpdPutCore1}};
         {dw, Idx, PutObj, _ReqId} ->
             %% Either returnbody is true or coord put merged with the existing
             %% object and bumped the vclock.  Either way use the returned
             %% object for the remote vnode
             ?DTRACE(?C_PUT_FSM_WAITING_LOCAL_VNODE, [2],
                     [integer_to_list(Idx)]),
             execute_remote(StateData#state{robj = PutObj, putcore = UpdPutCore1});

         %% dwが来た場合はlocalでの書き込みが完了しているので、remoteに結果をマージさせる
         {dw, Idx, _ReqId} ->
             %% Write succeeded without changes to vclock required and returnbody false
             ?DTRACE(?C_PUT_FSM_WAITING_LOCAL_VNODE, [2],
                     [integer_to_list(Idx)]),
             execute_remote(StateData#state{putcore = UpdPutCore1})
     end.

- riak_kv_vnode:coord_putの返すメッセージがw/dwかで遷移先が変わる
- dwは最低でも1に設定される(configで0に設定していても。validate内、278行目)
- よって、実書き込みが最低一回は行われる

``riak_kv_put_fsm:execute_remote/1``::

 execute_remote(StateData=#state{robj=RObj, req_id = ReqId,
                                 preflist2 = Preflist2, bkey=BKey,
                                 coord_pl_entry = CoordPLEntry,
                                 vnode_options=VnodeOptions,
                                 putcore=PutCore,
                                 starttime = StartTime}) ->
     StateData1 = add_timing(execute_remote, StateData),
     Preflist = [IndexNode || {IndexNode, _Type} <- Preflist2,
                              IndexNode /= CoordPLEntry],
     Ps = [[atom2list(Nd), $,, integer_to_list(Idx)] ||
              {Idx, Nd} <- lists:sublist(Preflist, 4)],
     ?DTRACE(?C_PUT_FSM_EXECUTE_REMOTE, [], [Ps]),
     riak_kv_vnode:put(Preflist, BKey, RObj, ReqId, StartTime, VnodeOptions),
     case riak_kv_put_core:enough(PutCore) of
         true ->
             {Reply, UpdPutCore} = riak_kv_put_core:response(PutCore),
             process_reply(Reply, StateData#state{putcore = UpdPutCore});
         false ->
             new_state(waiting_remote_vnode, StateData1)
     end.

- remoteで書き込みを行うべく、riak_kv_vnode:putに対して書き込みするRObjを渡す
- riak_kv_put_core:enough/1で、w, dw, pwの値を判定、終了条件を満たしていれば、process_replyで返事をする。
- なぜenoughでチェックしてからput呼ぶという流れではないのか？

  - riak_core_vnode_master:command側で、Preflist2からheadして投げる先のremoteを判断している
  - command内でPreflist2に対してリクエストを飛ばすループを回している

- 終了でなければ、remoteでの書き込みを待つ

``riak_kv_put_core:enough/1``::

 %% Check if enough results have been added to respond
 -spec enough(putcore()) -> boolean().
 %% The perfect world, all the quorum restrictions have been met.
 enough(#putcore{w = W, num_w = NumW, dw = DW, num_dw = NumDW, pw = PW, num_pw = NumPW}) when
       NumW >= W, NumDW >= DW, NumPW >= PW ->
     true;
 %% Enough failures that we can't meet the PW restriction
 enough(#putcore{ num_fail = NumFail, pw_fail_threshold = PWFailThreshold}) when
       NumFail >= PWFailThreshold ->
     true;
 %% Enough failures that we can't meet the DW restriction
 enough(#putcore{ num_fail = NumFail, dw_fail_threshold = DWFailThreshold}) when
       NumFail >= DWFailThreshold ->
     true;
 %% We've received all DW responses but can't satisfy PW
 enough(#putcore{n = N, num_dw = NumDW, num_fail = NumFail, pw = PW, num_pw = NumPW}) when
       NumDW + NumFail >= N, NumPW < PW ->
     true;
 enough(_PutCore) ->
     false.

``riak_kv_put_fsm:waiting_remote_vnode/1``::

 waiting_remote_vnode(Result, StateData = #state{putcore = PutCore}) ->
     ShortCode = riak_kv_put_core:result_shortcode(Result),
     IdxStr = integer_to_list(riak_kv_put_core:result_idx(Result)),
     ?DTRACE(?C_PUT_FSM_WAITING_REMOTE_VNODE, [ShortCode], [IdxStr]),
     UpdPutCore1 = riak_kv_put_core:add_result(Result, PutCore),
     case riak_kv_put_core:enough(UpdPutCore1) of
         true ->
             {Reply, UpdPutCore2} = riak_kv_put_core:response(UpdPutCore1),
             process_reply(Reply, StateData#state{putcore = UpdPutCore2});
         false ->
             {next_state, waiting_remote_vnode, StateData#state{putcore = UpdPutCore1}}
     end.

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
 
riak_core_vnode_master:command/4
================================

``riak_core_vnode_master:command``::

 %% Send the command to the preflist given with responses going to Sender
 command([{Index, Pid}|Rest], Msg, Sender, VMaster) when is_pid(Pid) ->
     gen_fsm:send_event(Pid, make_request(Msg, Sender, Index)),
     command(Rest, Msg, Sender, VMaster);

- riak_core_vnode_master:command側で、Preflist2からheadして投げる先のremoteを判断している
- command内でPreflist2に対してリクエストを飛ばすループを回している
- send_eventで呼び出すPidはriak_core_vnode。
- riak_core_vnodeはactiveになってるはずなので、riak_core_vnode:active(?VNODE_REQ{},State)にマッチする。

  - ?VNODE_REQは#riak_vnode_req_v1
  - make_requestは#riak_vnode_req_v1を返す

riak_core_vnoder:active/2
================================

``riak_core_vnode:active/2``::

 active(?VNODE_REQ{sender=Sender, request=Request},
        State=#state{handoff_node=HN}) when HN =:= none ->
     vnode_command(Sender, Request, State);
 active(?VNODE_REQ{sender=Sender, request=Request},State) ->
     vnode_handoff_command(Sender, Request, State);

``riak_core_vnode:vnode_command/3``::

 vnode_command(Sender, Request, State=#state{index=Index,
                                             mod=Mod,
                                             modstate=ModState,
                                             forward=Forward,
                                             pool_pid=Pool}) ->
     %% Check if we should forward
     case Forward of
         undefined ->
             Action = Mod:handle_command(Request, Sender, ModState);
         NextOwner ->
             lager:debug("Forwarding ~p -> ~p: ~p~n", [node(), NextOwner, Index]),
             riak_core_vnode_master:command({Index, NextOwner}, Request, Sender,
                                            riak_core_vnode_master:reg_name(Mod)),
             Action = continue
     end,
         ~~~~~~~~~~~~~~~~
     end.

- Modはriak_kv_vnodeなので、riak_kv_vnode:handle_command/3が呼び出される

riak_kv_vnode:handle_command/3
==============================

多分、?KV_PUT_REQにマッチするはず。

``riak_kv_vnode:handle_command/3``::

 handle_command(?KV_PUT_REQ{bkey=BKey,
                            object=Object,
                            req_id=ReqId,
                            start_time=StartTime,
                            options=Options},
                Sender, State=#state{idx=Idx}) ->
     StartTS = os:timestamp(),

     %% 一度、replyをwとして返してしまう
     riak_core_vnode:reply(Sender, {w, Idx, ReqId}),

     UpdState = do_put(Sender, BKey,  Object, ReqId, StartTime, Options, State),
     update_vnode_stats(vnode_put, Idx, StartTS),
     {noreply, UpdState};

- riak_core_vnodeに対して、書き込みリクエストは受け取ったぜ(ｷﾘｯ と、Wをreplyする。

  - replyによって、putcore中のw値が++される
  - でも、リクエスト受け取っただけでまだ書き込んだわけではない
  - 実際の書き込みはdo_put内で

``riak_kv_vnode:do_put/7``::

 %% @private
 %% upon receipt of a client-initiated put
 do_put(Sender, {Bucket,_Key}=BKey, RObj, ReqID, StartTime, Options, State) ->
     ~~~~~~~~~~~~~~~~
     Coord = proplists:get_value(coord, Options, false),
     PutArgs = #putargs{returnbody=proplists:get_value(returnbody,Options,false) orelse Coord,
                        coord=Coord,
                        lww=proplists:get_value(last_write_wins, BProps, false),
                        bkey=BKey,
                        robj=RObj,
                        reqid=ReqID,
                        bprops=BProps,
                        starttime=StartTime,
                        prunetime=PruneTime},
     {PrepPutRes, UpdPutArgs} = prepare_put(State, PutArgs),
     {Reply, UpdState} = perform_put(PrepPutRes, State, UpdPutArgs),

     %% riak_core_vnodeに、dwをお知らせ
     riak_core_vnode:reply(Sender, Reply),
 
     update_index_write_stats(UpdPutArgs#putargs.is_index, UpdPutArgs#putargs.index_specs),
     UpdState.

``riak_kv_vnode:perform_put/3``::

 perform_put({true, Obj},
             #state{idx=Idx,
                    mod=Mod,
                    modstate=ModState}=State,
             #putargs{returnbody=RB,
                      bkey={Bucket, Key},
                      reqid=ReqID,
                      index_specs=IndexSpecs}) ->
     Val = term_to_binary(Obj),
     case Mod:put(Bucket, Key, IndexSpecs, Val, ModState) of
         {ok, UpdModState} ->
             update_hashtree(Bucket, Key, Val, State),
             case RB of
                 true ->
                     Reply = {dw, Idx, Obj, ReqID};
                 false ->
                     Reply = {dw, Idx, ReqID}
             end;
         {error, _Reason, UpdModState} ->
             Reply = {fail, Idx, ReqID}
     end,
     {Reply, State#state{modstate=UpdModState}}.

- Modは、initで設定した`Mod = app_helper:gen_env(riak_kv, storage_backend)`が入る。
- ここでは、`riak_kv_bitcask_backend`を見る。

riak_kv_bitcask_backend:put/5
=============================

``riak_kv_bitcask_backend:put/5``::

 %% @doc Insert an object into the bitcask backend.
 -type index_spec() :: {add, Index, SecondaryKey} | {remove, Index, SecondaryKey}.
 -spec put(riak_object:bucket(), riak_object:key(), [index_spec()], binary(), state()) ->
                  {ok, state()} |
                  {error, term(), state()}.
 put(Bucket, PrimaryKey, _IndexSpecs, Val, #state{ref=Ref}=State) ->
     BitcaskKey = term_to_binary({Bucket, PrimaryKey}),
     case bitcask:put(Ref, BitcaskKey, Val) of
         ok ->
             {ok, State};
         {error, Reason} ->
             {error, Reason, State}
     end.

``bitcask:put/3``::

 %% @doc Store a key and value in a bitcase datastore.
 -spec put(reference(), Key::binary(), Value::binary()) -> ok.
 put(Ref, Key, Value) ->
     #bc_state { write_file = WriteFile } = State = get_state(Ref),
 
     %% Make sure we have a file open to write
     case WriteFile of
         undefined ->
             throw({error, read_only});
 
         _ ->
             ok
     end,
 
     {Ret, State1} = do_put(Key, Value, State, ?DIABOLIC_BIG_INT, undefined),
     put_state(Ref, State1),
     Ret.

bitcask:do_put/5
=================

``bitcask:do_put/5``::

 %% Internal put - have validated that the file is opened for write
 %% and looked up the state at this point
 do_put(_Key, _Value, State, 0, LastErr) ->
     {{error, LastErr}, State};
 do_put(Key, Value, #bc_state{write_file = WriteFile} = State, Retries, _LastErr) ->

         ~~~~~~~~~~~~~~~~

     case bitcask_nifs:keydir_put(State2#bc_state.keydir, Key,
                                  bitcask_fileops:file_tstamp(WriteFile2),
                                  Size, Offset, Tstamp, true) of
         ok ->
             {ok, State2#bc_state { write_file = WriteFile2 }};
         already_exists ->
             ~~~~~~~~~~~~
     end.

``bitcask_nifs:keydir_put/9``::

 keydir_put(Ref, Key, FileId, TotalSz, Offset, Tstamp, NewestPutB,
            OldFileId, OldOffset) ->
     keydir_put_int(Ref, Key, FileId, TotalSz, <<Offset:64/unsigned-native>>,
                    Tstamp, if not NewestPutB -> 0;
                               true           -> 1
                            end,
                    OldFileId, <<OldOffset:64/unsigned-native>>).
 
 keydir_put_int(_Ref, _Key, _FileId, _TotalSz, _Offset, _Tstamp, _NewestPutI,
                _OldFileId, _OldOffset) ->
     erlang:nif_error({error, not_loaded}).

- 書き込み処理がどこにもないように見える...
- 裏側でCのプロセスをコールしている
- -on_load()でCとの紐付けがなされる

``bitcask_nifs:init/0``::

 init() ->
     case code:priv_dir(bitcask) of
         {error, bad_name} ->
             case code:which(?MODULE) of
                 Filename when is_list(Filename) ->
                     SoName = filename:join([filename:dirname(Filename),"../priv", "bitcask"]);
                 _ ->
                     SoName = filename:join("../priv", "bitcask")
             end;
          Dir ->
 			SoName = filename:join(Dir, "bitcask")
     end,
     erlang:load_nif(SoName, 0).

- filenameを.soと紐付けている

``c_src/bitcask_nifs.c``::

 ERL_NIF_INIT(bitcask_nifs, nif_funcs, &on_load, NULL, NULL, NULL);

- 1951行目で紐付け

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
