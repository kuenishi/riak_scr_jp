# ets and folsom by @kuenishi

# ets

- Erlang/OTP 組み込みのオンメモリKVS
- Cでアトミック命令とかを使ってロックフリーに実装
- データは全てコピー（…と思いきや、大きなバイナリはポインタだけ）
- ロックフリーなので複数の軽量プロセスから同時アクセスしても安全

## example

構想30秒、この日のために書き上げた `basho_bench_driver_ets.erl`

```erlang
new(_Id) ->
    Opts = basho_bench_config:get(options, ?MODULE),
    Tid = ets:new(?MODULE, Opts),
    {ok, Tid}.

run(get, KeyGen, _ValueGen, State) ->
    Tid = State,
    Key = KeyGen(),
    case ets:lookup(Tid, Key) of
        [] ->
            {ok, State};
        [{Key, _}] ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(put, KeyGen, ValueGen, State) ->
    Tid = State,
    true = ets:insert(Tid, {KeyGen(), ValueGen()}),
    {ok, State};
run(delete, KeyGen, _ValueGen, State) ->
    Tid = State,
    true = ets:delete(Tid, KeyGen()),
    {ok, State}.
```

オプション例＞

```
spam = ets:new(spam, [public, ordered_set])
```

詳しくは [`erl -man ets`](http://erlang.org/doc/man/ets.html#new-2)

## ets ソース案内

```
$ git clone git://github.com/erlang/otp
```

- `lib/stdlib/src/ets.erl` <- ちがうっぽい
- `erts/emulator/beam/erl_db.c` <- これっぽい

### `erts/emulator/beam/erl_db_*`

- `DbTerm`
- `DbTableMethod`
- `DbTableRelease`
- `DbTableCommon`

```c
union db_table {
    DbTableCommon common; /* Any type of db table */
    DbTableHash hash;     /* Linear hash array specific data */
    DbTableTree tree;     /* AVL tree specific data */
    DbTableRelease release;
    /*TT*/
};
```

絵にすると

```
+-----------------+
|  DbTableCommon  |
|  ....           |
|  ....           |
+-----------------+
|                 |
| extra space     |
|                 |
+-----------------+
```

注目のメンバ

```c
typedef struct db_table_common {
    ...
    DbTableMethod* meth;      /* table methods */
    ...
    Uint32 status;            /* bit masks defined  below */
    ...
} DbTableCommon;
```

`erl_db.c` での `status` や `meth`

```c
	if (val == am_bag) {
	    status |= DB_BAG;
	    status &= ~(DB_SET | DB_DUPLICATE_BAG | DB_ORDERED_SET);
	}
	else if (val == am_duplicate_bag) {
	    status |= DB_DUPLICATE_BAG;
	    status &= ~(DB_SET | DB_BAG | DB_ORDERED_SET);
	}
	else if (val == am_ordered_set) {
	    status |= DB_ORDERED_SET;
	    status &= ~(DB_SET | DB_BAG | DB_DUPLICATE_BAG);
	}
    ...
    if (IS_HASH_TABLE(status)) {
	meth = &db_hash;
#ifdef ERTS_SMP
	if (is_fine_locked && !(status & DB_PRIVATE)) {
	    status |= DB_FINE_LOCKED;
	}
#endif
    }
    else if (IS_TREE_TABLE(status)) {
	meth = &db_tree;
    }

```

↑の `status` で、 `DB_SET` `DB_BAG` `DB_ORDERED_SET` `DB_DUPLICATE_BAG` を分けている。

```c
BIF_RETTYPE ets_insert_2(BIF_ALIST_2)
{
    ...
    meth = tb->common.meth;
    ...
    cret = meth->db_put(tb, CAR(list_val(lst)), 0);
```

ポリモーフィズム！！
`DbTableHash` だと `db_put_hash` という関数になっている模様

ソース案内図（※想像です

+-------------+-----------------------------+
|erl_db.c     | DBのインターフェース実装    |
+-------------+-----------------------------+
|erl_db.h     | 特に何も。メモリ関連ぽい？  |
+-------------+-----------------------------+
|erl_db_hash.h| set, bag のクラス宣言       |
+-------------+-----------------------------+
|erl_db_hash.c|                             |
+-------------+-----------------------------+
|erl_db_tree.h| ordered_set?                |
+-------------+-----------------------------+
|erl_db_tree.c|                             |
+-------------+-----------------------------+
|erl_db_util.h| 便利関数がもろもろ定義      |
+-------------+-----------------------------+
|erl_db_util.c| ぽりもーふぃずむ            |
+-------------+-----------------------------+

concurrencyまわりに更に興味があるひとは `erl_threads.h` をみると非常にドキュメントもかいてあります

## 性能

```
processor       : 7
vendor_id       : GenuineIntel
cpu family      : 6
model           : 58
model name      : Intel(R) Core(TM) i7-3770 CPU @ 3.40GHz
stepping        : 9
microcode       : 0x12
cpu MHz         : 3401.000
cache size      : 8192 KB
```

```
$ head /proc/meminfo 
MemTotal:       32718832 kB
...
```

(R,W,D)=(10,10,1) でだいたい 600k qps

```
                                 Throughput ops/sec

    2e+06 ++---------+----------+----------+---------+----------+---------++
          +          +          +          +         +          +          +
  1.8e+06 +A..............................................................++
  1.6e+06 +*..............................................................++
          | *        :          :          :         :          :          |
  1.4e+06 ++*.............................................................++
          | *        :          :          :         :          :          |
  1.2e+06 ++.*............................................................++
    1e+06 ++.*.......................................A....................++
          |  *       :          :          :  A      **         :          |
   800000 ++.*.........................A.....*.*....*.*...................++
          |   * *A** : A**    A**    ** *  : *  *   *: *      *A**         |
   600000 ++..A*.....**...*.**.....*A....*..*....*..*...*...A*....A**.....++
          |         A:     A    :A*       *:*     ** :   * *    :    A     |
   400000 ++...............................A.......Acurrent - total.**A***++
   200000 ++.......................................current - failed ##B###++
          +          +          +          +         +          +          +
        0 +B##B##B##B##B###B##B##B##B##B###B##B####B#B####B#B##B##B##B----++
          0         100        200        300       400        500        600
                                    Elapsed [sec]
```

## 使い方の注意

- 複数行を安全に更新できない
- レコードを消し忘れない

# folsom

- [Metrics](http://metrics.codahale.com) というJavaのライブラリのErlang版

- metric types
- Counters, Gauges, Histgrams (Timers) - Uniform, Exdec, Slide, Slide_uniform
- Histories, Meters - Spiral, Meter-readers,
- metrics grouping

```
$ find . -type f | egrep "erl$"| grep -v test | grep -v "deps/folsom"| grep -v "eunit" | xargs grep folsom  | wc
     139     837   15713
```

riak_api, riak_core, riak_kv, riak_pipe, riak_search

http://localhost:8098/stats の内容

```
curl http://riak.kuenishi.net/stats | python -mjson.tool
```

- `riak_kv_wm_stats:get_stats/0`

 - `riak_kv_stat:get_stats/0`
 - `riak_core_stat:get_stats/0`

`riak_core_stat_cache` -> `folsom_metrics`

基本的には計算してから入れるモデル。 `get_value` は値をとりだすだけ。

```
?FOLSOM_TABLE
  Name -> #metric{type = counter | gauge | histogram | duration ...}

?COUNTER_TABLE
  {{Name, 0..15}, 0}
  -> ets:update_counter/3 スケジューラ毎にアクセスするエントリを分けている

key(Name) ->
    X = erlang:system_info(scheduler_id),
    Rnd = X band (?WIDTH-1),
    {Name, Rnd}.

?GAUGE_TABLE -> ...
```

`riak_kv_stat:do_update/1` をみると、どのメトリックスを更新しているか分かる。
