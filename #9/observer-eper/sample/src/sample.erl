-module(sample).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, locked/2, unlocked/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-export([stop/0, push/0, coin/1]).

-record(state, {count=0}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_fsm:send_all_state_event(?SERVER, stop).

push() ->
    gen_fsm:send_event(?MODULE, {push}).

coin(Cnt) ->
    gen_fsm:send_event(?MODULE, {coin, Cnt}).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, locked, #state{}}.

%% Lock中はpush出来ない
locked({push},State) ->
    log("Enter 3 coins"),
    {next_state, locked, State};
%% coinが3枚以上でLockを解除
locked({coin, Cnt}, #state{count=CurrentCnt} = State) when CurrentCnt + Cnt >= 3 ->
    log("Freed lock. You can push."),
    {next_state, unlocked, State#state{count=0}, 3000};
%% coinは加算
locked({coin, Cnt}, #state{count=CurrentCnt} = State) ->
    io:format("~p coins~n", [Cnt + CurrentCnt]),
    {next_state, locked, State#state{count=CurrentCnt + Cnt}}.

%% unlock中にpushで再度Lock
unlocked({push}, State) ->
    log("Locked."),
    {next_state, locked, State};
%% unlock中でもcoinは加算し続ける
unlocked({coin, Cnt}, #state{count=CurrentCnt} = State) ->
    io:format("~p coins~n", [Cnt + CurrentCnt]),
    {next_state, unlocked, State#state{count=CurrentCnt + Cnt}, 3000};
%% timeoutしたらlockedに戻る
unlocked(timeout, State) ->
    {next_state, locked, State}.

handle_event(stop, _StateName, State) ->
    log("Received stop message."),
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

log(Msg) ->
    io:format("~ts~n", [Msg]).
