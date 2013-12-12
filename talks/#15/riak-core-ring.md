# Riak Core: Ring (1/N: N is unknown)

- riak: commit=`a12d5a2` riak-2.0.0pre7
- riak_core: commit=`4dc9459`

## 概要

- riak core の第1回
- Ring まわりのコードをざっと眺める

何回かに渡り...
- ノード join/leave, ノード障害時の挙動
- ring metadata とは、どう伝播して、どう reconcile されるか
- ownership handoff 中の coverage operation も関係してくるかな(?)

## TODO

- tainted ring ってなに?
- ring_update event の処理

## `*ring*` なモジュール

- `riak_core_ring.erl`
  - ring のデータ構造
  - 状態は持たない
- `riak_core_ring_manager.erl`
  - `gen_server` callback
  - ring の状態管理
  - API: `get_my_ring/set_my_ring`, `get_ring_id`, `get_bucket_meta`,
    `write_ring_file`, ...
- `riak_core_ring_events.erl`
  - `gen_event` callback
  - API: `add_*_handler`, `add_*_callback`, `*_update`
  - まだわからん
- `riak_core_ring_handler.erl`
  - `gen_event` callback
  - `handle_event({ring_update, ...})`
- `riak_core_ring_util.erl`
  - utility functions
  - 59 lines, ちっちゃい
  - API: `assign`, `check_ring`

## `riak_core_sup.erl`

```
init([]) ->
    [snip]
              ?CHILD(riak_core_ring_events, worker),
              ?CHILD(riak_core_ring_manager, worker),
    [snip]
{ok, {{one_for_one, 10, 10}, Children}}.
```

## Data Structures

### `riak_core_ring.hrl`

```
-define(LEGACY_RING_VSN, 1).
-define(CURRENT_RING_VSN, 2).
```

### `riak_core_ring.erl` レコード

chstate: (consistent hashing state)

```
-define(CHSTATE, #chstate_v2).
-record(chstate_v2, {
    nodename :: term(),          % the Node responsible for this chstate
    vclock   :: vclock:vclock(), % for this chstate object, entries are
                                 % {Node, Ctr}
    chring   :: chash:chash(),   % chash ring of {IndexAsInt, Node} mappings
    meta     :: dict(),          % dict of cluster-wide other data (primarily
                                 % bucket N-value, etc)

    clustername :: {term(), term()},
    next     :: [{integer(), term(), term(), [module()], awaiting | complete}],
    members  :: [{node(), {member_status(), vclock:vclock(), [{atom(), term()}]}}],
    claimant :: term(),
    seen     :: [{term(), vclock:vclock()}],
    rvsn     :: vclock:vclock()
}).
```

```
-type member_status() :: joining | valid | invalid | leaving | exiting | down.
```

```
%% type meta_entry(). Record for each entry in #chstate.meta
-record(meta_entry, {
    value,    % The value stored under this entry
    lastmod   % The last modified time of this entry, 
              %  from calendar:datetime_to_gregorian_seconds(
              %                             calendar:universal_time()), 
}).
```

exported types

```
%% riak_core_ring() is the opaque data type used for partition ownership
-type riak_core_ring() :: ?CHSTATE{}.
-type chstate() :: riak_core_ring().

-type pending_change() :: {Owner :: node(),
                           NextOwner :: node(),
                           awaiting | complete}
                        | {undefined, undefined, undefined}.
```

Example: 4 node, 16 ring size でクラスタ組んだ時のデータ `chstate`
https://gist.github.com/shino/800f63583ec71c8037a7

## `riak_core_ring.erl` で肩慣らし (深入りせずに)

Ring 取得

```
> {ok, R} = riak_core_ring_manger:get_my_ring().
```

Some API calls

`-spec all_members(State :: chstate()) -> [Node :: term()].`
```
> riak_core_ring:all_members(R).
['dev1@127.0.0.1','dev2@127.0.0.1','dev3@127.0.0.1',
 'dev4@127.0.0.1']
```



`-spec all_owners(State :: chstate()) -> [{Index :: integer(), Node :: term()}].`
```
> riak_core_ring:all_owners(R).
[{                                               0, 'dev1@127.0.0.1'},
 { 91343852333181432387730302044767688728495783936, 'dev2@127.0.0.1'},
 {182687704666362864775460604089535377456991567872, 'dev3@127.0.0.1'},
 {274031556999544297163190906134303066185487351808, 'dev4@127.0.0.1'},
 {365375409332725729550921208179070754913983135744, 'dev1@127.0.0.1'},
 {456719261665907161938651510223838443642478919680, 'dev2@127.0.0.1'},
 [snip]
```

`pritty_print`

```
> riak_core_ring:pretty_print(
     element(2,riak_core_ring_manager:get_raw_ring()), [legend]).
==================================== Nodes ====================================
Node a: 4 ( 25.0%) dev1@127.0.0.1
Node b: 4 ( 25.0%) dev2@127.0.0.1
Node c: 4 ( 25.0%) dev3@127.0.0.1
Node d: 4 ( 25.0%) dev4@127.0.0.1
==================================== Ring =====================================
abcd|abcd|abcd|abcd|
```

`-spec is_primary(chstate(), {integer(), node()}) -> boolean().`
```
> Width = trunc(math:pow(2, 160)/16).
> riak_core_ring:is_primary(R, {0*Width, node()}).
true
> riak_core_ring:is_primary(R, {1*Width, node()}).
false
> riak_core_ring:is_primary(R, {3*Width, node()}).
false
> riak_core_ring:is_primary(R, {4*Width, node()}).
true
```

## `riak_core_ring_manger.erl`

```
-define(RING_KEY, riak_ring).
-define(ETS, ets_riak_core_ring_manager).

%% @spec get_my_ring() -> {ok, riak_core_ring:riak_core_ring()} | {error, Reason}
get_my_ring() ->
    Ring = case riak_core_mochiglobal:get(?RING_KEY) of
               ets ->
                   case ets:lookup(?ETS, ring) of
                       [{_, RingETS}] ->
                           RingETS;
                       _ ->
                           undefined
                   end;
               RingMochi ->
                   RingMochi
           end,
    case Ring of
        Ring when is_tuple(Ring) -> {ok, Ring};
        undefined -> {error, no_ring}
    end.
```

なにはともあれ `start_link` , `init`
```
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [live], []).
```

```
%% Mode = live
init([Mode]) ->
    setup_ets(Mode),
    Ring = reload_ring(Mode),
    State = set_ring(Ring, #state{mode = Mode}),
    riak_core_ring_events:ring_update(Ring),
    {ok, State}.
```
