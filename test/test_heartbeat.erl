-module(test_heartbeat).
-author('Joseph Abrahamson <me@jspha.com>').

-behaviour(gen_heartbeat).

-export([init/1, terminate/2, beat/1, inspect/1, code_change/3]).


init(_) -> 
    {ok, 2000, undefined, [{type, linear}]}.

beat(_) ->
    io:fwrite("hi"),
    timer:sleep(1000),
    ok.

inspect(_State) -> ok.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
