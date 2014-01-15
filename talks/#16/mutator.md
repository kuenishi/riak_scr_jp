# Mutator (riak-2.0.0pre10)

## What

Get/Put時にvnodeでbackendの手前(riak_kv_vnode)でデータ(riak_object)を変化させる為のhook. (replica毎)

Pre/Post-CommitHookはriak_kv_[get|put]_fsmのフック

- Mutator
  - vnode/replica毎
  - responseの操作はできない
- Commit Hook
  - request毎
  - responseの操作ができる

## Why

```
%% @doc There are circumstances where the object stored on disk is not
%% the object to return; and there are times the object written to the
%% data storage backend is not meant to be the object given. An
%% example would be storing only meta data for an object on a remote
%% cluster. This module is an interface to register mutators that will
%% be run.
```

## Demo

## riak_kv_mutator

mutatorの登録、管理モジュール
riak_kv_mutator callbackの定義

- register/1, register/2
- unregister/1
- get/0
- mutate_get/1              ---->   Module:mutate_get/1
- mutate_put/2, mutate_put3 ---->   Module:mutate_put/5

mutate_put(Meta, Value, ExposedMeta, RObject, BucketProps)

siblingごとのmetadata, valueを個々に操作。
returnbody専用データはExposedMetaへ入れる。

## Usage

- `mutate_put/5`, `mutate_get/1`を実装したモジュールを用意
- moduleを登録 `riak_kv_mutator:regirster/1, 2`

## Sources

- riak_kv_vnode.erl
- riak_kv_mutator.erl

### riak_kv_vnode

```
perform_put({true, Obj},
            State,
            #putargs{returnbody=RB,
                     bkey=BKey,
                     bprops=BProps,
                     reqid=ReqID,
                     index_specs=IndexSpecs}) ->

    %% Avoid the riak_kv_mutator code path if no mutators are
    %% registered.
    {Obj2, Fake} = case riak_kv_mutator:get() of
        {ok, []} ->
            {Obj, Obj};
        {ok, Mutators} ->
            riak_kv_mutator:mutate_put(Obj, BProps, Mutators)
    end,

    {Reply, State2} = actual_put(BKey, Obj2, Fake, IndexSpecs, RB, ReqID, State),
    {Reply, State2}.
```

```
actual_put(BKey={Bucket, Key}, Obj, MaybeFake, IndexSpecs, RB, ReqID,
           State=#state{idx=Idx,
                        mod=Mod,
                        modstate=ModState}) ->
    case encode_and_put(Obj, Mod, Bucket, Key, IndexSpecs, ModState) of
        {{ok, UpdModState}, _EncodedVal} ->
            update_hashtree(Bucket, Key, Obj, State),
            maybe_cache_object(BKey, MaybeFake, State),
            ?INDEX(Obj, put, Idx),
            case RB of
                true ->
                    Reply = {dw, Idx, MaybeFake, ReqID};
                false ->
                    Reply = {dw, Idx, ReqID}
            end;
        {{error, Reason, UpdModState}, _EncodedVal} ->
            Reply = {fail, Idx, Reason}
    end,
    {Reply, State#state{modstate=UpdModState}}.

```

## riak_kv_mutator

ソース
