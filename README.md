# `gen_heartbeat`: a little server for repeating events

For times when `timer:send_after` needs a bit more supervision.

Suppose we have `a_module.erl`

```erlang
-module(a_module).

-behaviour(gen_heartbeat).

init(_Args) -> {ok, 2000, undefined, [{type, linear}, {name, "cardio"}]}.
terminate(_Reason, undefined) -> ok.
code_change(_Vsn, undefined, _Extra) -> {ok, State}.
inspect(undefined) -> ok.

beat(undefined) ->
  lager:info("lub, dub...").
```

```erlang
1> {ok, Heart} = gen_heartbeat:start_link(a_module, []).
2> lager:start().

22:46:43.823 [info] lub, dub...
22:46:44.319 [info] cardio: heartbeat in 0.100400ms
```

