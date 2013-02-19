!SLIDE

# OTP入門 gen\_server編

!SLIDE

## 今日の話

- OTP
- OTP behaviour
- gen\_server
- APIの設計例
- rebarを使ったdemo

!SLIDE

## OTP(Open Telecom Platform)とは

_OTP は一種のアプリケーションオペレーションシステムで、大規模、耐障害、分散といった特徴を持つアプリケーションを作るためのライブラリや手続きの集合体だ。 ~ プログラミングErlang(飛行機本)  Joe Armstrong_

!SLIDE

## OTP(Open Telecom Platform)に含まれるもの

- Erlangの標準ライブラリ(io, lists, calendar)
- OTP behaviour(後ほど)
- Testライブラリ(eunit, common\_test)
- ミドルウェア(mnesia)
- Tool(edoc, appmon, xref, profiler)
- Run-Time

!SLIDE

## OTP behaviour

Erlangで開発する際によく使うパターンをフレームワーク化したもの

- application: 起動・停止
- superviour:  プロセスの監視・再起動
- gen\_server: 汎用サーバ
- gen\_fsm:    汎用有限状態マシン
- gen\_event:  汎用イベントハンドラ

<br/>

@@@ erlang

    -module(sample_server).
    -behaviour(gen_server).
    ....
@@@

!SLIDE

## Erlangアプリケーションの構成例(supervision tree)

      riak_kv_app
      (application)
            |
            +----- riak_kv_sup
                  (supervisor)
                         |
                         +- riak_kv_js_sup --- riak_kv_js_vm
                         |  (supervisor)       (gen_server)
                         |
                         +- riak_kv_vnode_master
                         |     (gen_server)
      webmachine_app     +-....
      (application)
            |
            +- webmachine_sup
!SLIDE

# appmon

!SLIDE

## ということで OTP入門 gen\_server編

!SLIDE

## gen_serverとは

サーバの汎用的な機能とインターフェース定義を持ち合わせたモジュール。

- プロセス管理(生成)
- メッセージ送受信制御(sync/async/timeout)
- 共通インターフェース定義(init/0, handle\_call/3...)

<br/>

@@@ erlang

    gen_server:start_link/4
    gen_server:call/2

    Module:init/0
    Module:handle_call/3
@@@

!SLIDE

## gen_serverで何が嬉しいのか

サーバプロセスを作るときにいつも書く処理を書かなくてよい

- spawn
- init
- recevie
- loop

!SLIDE

## サーバプロセスを作るときにいつも書く処理？

!SLIDE

## 前提知識：Erlangのプロセス操作

<br>

#### プロセス生成とメッセージ送信

@@@ erlang

    Pid = spawn_link(Function), % creating new process.
    Pid ! {self(), Msg},   % sending a message.
@@@

<br>

#### メッセージ受信と応答

@@@ erlang

    loop() ->
        receive
            {From, Msg} ->
                % doing something.
                From ! {self(), Response}
        end,
        loop().
@@@

!SLIDE

## 前提知識：Erlangのプロセス操作

#### プロセスの名前登録

@@@ erlang

    Pid = spawn_link(Function), % creating new process.
    register(process_name, Pid),
    process_name ! {self(), Msg},   % sending a message.
@@@

!SLIDE

## gen_server導入前

!SLIDE

## gen_server導入前(Server)

@@@ erlang

    start() ->
        State = init(),
        Pid = spawn_link(?MODULE, loop, [State]).
    init() ->
        State = do_something(),
        State.
    loop(State) ->
        receive
            {From, Msg} ->
                State1 = handle_xxx(),
                From ! {ok, Response},
                loop(State1)
        after 100000  ->
                exit({timeout, Reason})
        end.
@@@

!SLIDE

## gen_server導入前(Client)

@@@ erlang

    Pid = server:start(),
    Pid ! {self(), Msg},
@@@

!SLIDE

## gen_server導入後

!SLIDE

## gen_server導入後(Server)

@@@ erlang

    start() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    init([]) ->
        State = do_something,
        State.
    handle_call(Msg, From, State) ->
        Res = handle_xxxx(),
        {reply, Res, State}.
@@@

!SLIDE

## gen_server導入後(Client)

@@@ erlang

    server:start(),
    gen_server:call(?MODULE, Msg).
@@@

!SLIDE

### コードがシンプルになり、
### 受信メッセージをどう処理するかに注力できます

!SLIDE

## API設計の例

!SLIDE

## API設計の例

- プロセスの隠蔽化
- 同一モジュール内にClient/Serverを実装(無論case-by-case)

<br/>

こんなAPIはさすがに書かない

@@@ erlang

    xx_storage ! {self(), {set, Key, Value}}
    xx_storage ! {self(), {get, key, Value}}
@@@

!SLIDE

## API設計の例

@@@ erlang

    handle_call(From, {get, Key}, State) ->
        {reply, do_get(Key), State};
    handle_call(From, {set, Key, Value}, State) ->
        {reply, do_set(Key, Value), State}.

    %% internal API
    do_get(Key) ->
        %% get operation
        Value.
    %% API
    get(Key) ->
        gen_server:call(?MODULE, {get, Key}).
@@@

!SLIDE

## API設計の例

予めサーバを起動

@@@ erlang

    xx_storage:start_link(),

@@@

<br>

APIを使用する

@@@ erlang

    xx_store:set(Key, Value),
    xx_store:get(Key),
@@@

!SLIDE

# rebarを使ったdemo

!SLIDE

## Demo: テンプレートの作成

@@@ sh

    ./rebar create-app appid=otp_intro
    ./rebar create template=simplesrv srvid=otp_intro
@@@

!SLIDE

## 実際のコードを眺めてみる

- `webmachine_router.erl`
- `riak_core_vnode_manager.erl`

!SLIDE

## gen_serverのfunctionとcallbackの対応

!SLIDE


## gen_serverのAPI

@@@

    gen_server:start_link -----> Module:init/1
@@@

<br>

プロセスの生成：それに伴う初期化処理

!SLIDE

## gen_serverのAPI

@@@

    gen_server:call -----> Module:handle_call/3
@@@

プロセスの同期呼び出し：それに対する応答

!SLIDE

## gen_serverのAPI

非同期処理

@@@

    gen_server:cast -----> Module:handle_cast/2
@@@

プロセスの非同期呼び出し：それに対する応答

!SLIDE

## gen_serverのAPI

@@@

    Module:handle_info/2
@@@

timeout等、別のメッセージを処理する応答

!SLIDE

## おまけ：debugオプション

@@@ erlang

    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{debug,[trace]}]).
@@@

!SLIDE

## オンラインの資料

- [Erlangユーザーズガイド日本語訳](http://erlang.shibu.jp/)
- [learn you some Erlang for great good日本語訳](http://www.ymotongpoo.com/works/lyse-ja/)

!SLIDE

# おわり
