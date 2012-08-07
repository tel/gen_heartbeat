-module(gen_heartbeat).
-compile([{parse_transform, lager_transform}]).
-author('Joseph Abrahamson <me@jspha.com>').

-export([start/2, start_link/2, 
         start/3, start_link/3,
         stop/1]).
-export([force/1, inspect/1]).
-export_type([option/0]).

-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3, 
         handle_cast/2, handle_info/2, code_change/3]).

%% @todo what is the type of tref()? {pos_int, ref}?
-type tref() :: {pos_integer, _}.

-record(st, {module :: module(),
             state  :: _,
             name   :: binary(),
             type   :: atom(),
             period :: pos_integer(),
             tref   :: tref()}).

%% ------------
%% Public types

-type option() :: 
{'name', Name :: iolist()} | 
{'type', 'linear' | 'parallel'}.

-type server() :: 
pid() | atom() | {atom(), atom()} | {global, atom()} | {via, module(), atom()}.

-type start_return() :: {ok, pid()} | ignore | {error, Error :: _}.


%% -------------------
%% Callback behaviours

-callback init(_) ->
    {'ok', Period :: pos_integer()} | 
        {'ok', Period :: pos_integer(), State :: _} | 
        {'ok', Period :: pos_integer(), State :: _, Options :: list(option())}.
%% Initializes a heartbeat server. A natural (state-less)
%% gen_heartbeat can be started with just {@type {ok, Period}} which
%% causes the `beat/1' function to be called every `Period'
%% milliseconds, passing `undefined' as the default state. Non-default
%% initialized state can be specified by returning {@type {ok,
%% Period, State}} where `State' is passed to `beat/1'
%% specifically. Finally, a number of options can be specified as a
%% proplist. {@type {name, Name}} primarily affects documentation and
%% logging via `zeta' and `lager'. {@type {type, parallel | linear}}
%% specifies whether the next beat occurs `Period' milliseconds after
%% <em>the previous beat</em> (`parallel') or `Period' milliseconds
%% after the previous <em>call</em> to `beat/1' <em>returns</em>
%% (`linear'). Note that under `linear' operation crashes that occur
%% in `beat/1' indicate the 

-callback terminate(Reason :: term(), State :: _) -> 'ok'.
%% A callback for performing teardown operations from the server
%% before the repeated calls stop. Periods will still occur until
%% <em>after</em> `terminate/1' returns.

-callback beat(State :: _) -> ok | {ok, NewState :: _}.
%% Performs a heartbeat. This core functionality is called in an
%% independently spawned process repeatedly by the `gen_heartbeat'
%% server, thus errors and ``'EXIT'''s are isolated to
%% `beat/1'. `beat/1' may return `NewState' to update the server's
%% state (although only `linear'-type `gen_heartbeat's are guaranteed
%% to pass the new state to the <em>next</em> call to
%% `beat/1').

-callback inspect(State :: _) -> _.
%% Converts the `State' to an "inspectable" form for analyzing
%% `gen_heartbeat's.

-callback code_change(Vsn :: nonempty_string(), State :: _, Extra :: _) ->
    {ok, State :: _}.
%% Callback for doing code changes, just like normal OTP.

%% -------------
%% Lifecycle API

-spec 
start(Module :: module(), Args :: list(option())) -> start_return().
start(Module, Args) -> 
    gen_server:start(?MODULE, [Module, Args], []).

-spec 
start(ServerName :: {local, atom()} | {global, atom()} | {via, module(), atom()}, 
      Module :: module(), 
      Args :: list(option())) -> start_return().
start(ServerName, Module, Args) -> 
    gen_server:start(ServerName, ?MODULE, [Module, Args], []).

-spec 
start_link(Module :: module(), Args :: list(option())) -> start_return().
start_link(Module, Args) -> 
    gen_server:start_link(?MODULE, [Module, Args], []).

-spec 
start_link(ServerName :: {local, atom()} | {global, atom()} | {via, module(), atom()}, 
           Module :: module(),
           Args :: list(option())) -> start_return().
start_link(ServerName, Module, Args) -> 
    gen_server:start_link(ServerName, ?MODULE, [Module, Args], []).

-spec
stop(Server :: server()) -> ok.
stop(Server) -> gen_server:call(Server, {stop, user}).


%% ----------
%% Public API

-spec 
force(Server :: server()) -> ok.
%% @doc 
%%
%% Forces a call to `beat/1' immediately. This occurs exactly as if it
%% were a normal beat with respect to state updates, command
%% processing, and future beats. In particular, `force/1' can be
%% dangerous if there is an expectation of beats being somehow
%% synchronized with other processes.
force(Server) -> Server ! beat, ok.

-spec
inspect(Server :: server()) -> 
    {ok, {Name :: binary(), 
          ToNext :: pos_integer(), 
          Period :: pos_integer(), 
          Pid :: pid(), 
          StateDescription :: _}}. 
%% @doc
%%
%% Inspects a running `gen_heartbeat' for its current state.
inspect(Server) -> gen_server:call(Server, inspect).


%% -------------------------
%% gen_server implementation

init([Module, Args]) ->
    {Period, State, Opts} = do_init(Module:init(Args)),
    Type = proplists:get_value(type, Opts, parallel),
    Name = proplists:get_value(name, Opts, <<"a gen_heartbeat server">>),
    {ok, TRef} = timer:send_after(Period, beat),
    {ok, #st{module = Module,
             state  = State,
             type   = Type,
             period = Period,
             name   = erlang:iolist_to_binary(Name),
             tref   = TRef}}.

do_init({ok, Period}) -> {Period, undefined, []};
do_init({ok, Period, State}) -> {Period, State, []};
do_init({ok, Period, State, Opts}) -> {Period, State, Opts}.

terminate(Reason, #st{tref = TRef, module = Module, state = MState}) -> 
    %% Fulfills the promise that `Module:terminate' is executed before
    %% the timers stop. Not sure what good that does, though, since
    %% it'll just freeze the server waiting for `Module:termiante' to
    %% finish and no new timers will be executed.
    %%
    %% @todo fix this behavior?
    Module:terminate(Reason, MState),
    {ok, cancel} = timer:cancel(TRef),
    ok.

handle_call({stop, user}, _From, State) -> {stop, {shutdown, user}, stopped, State};
handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call(_Msg, _From, State) -> {reply, ignored, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({update_state, NewMState}, State) -> {noreply, State#st{state = NewMState}};
handle_info(beat, 
            State0 = #st{tref = TRef0,
                         type = Type,
                         period = Period}) ->

    %% If this was a forced beat, we'll be sure to kill the currently
    %% incoming timer. Note that if it's already fired and is
    %% enqueued, there's nothing we can do! (Well, we could use Refs
    %% to track to see if it's a proper timer beat, but that's a lot
    %% more work).
    %% @todo ref-tracked timers?
    {ok, cancel} = timer:cancel(TRef0),
    
    {noreply, case Type of
                  %% For `parallel' types we start the next signal
                  %% timeout <em>immediately</em> and do all the state
                  %% updating asynchronously (see: `eval_beat_async/1').
                  parallel ->
                      {ok, NewTRef} = timer:send_after(Period, beat),
                      ok = eval_beat_async(State0),
                      State0#st{tref = NewTRef};
                  %% For `linear' types we evaluate the beat and wait
                  %% on its return (there's a clever trick to check
                  %% for failures/crashes, see `eval_beat/1'). After
                  %% the beat's finished, <em>then</em> we trigger the
                  %% next timeout.
                  linear -> 
                      {ok, NewState} = eval_beat(State0),
                      {ok, NewTRef} = timer:send_after(Period, beat),
                      NewState#st{tref = NewTRef}
              end};
handle_info(_Msg, State) -> {noreply, State}.

eval_beat(State = #st{name = Name, module = Module, state = MState0}) ->
    %% @todo This looks like a pattern which could easily be extracted
    %% into zeta.
    {MsTime, NewState} = 
        case timer:tc(Module, beat, [MState0]) of
            {RTime, ok} -> 
                {RTime/1000, State};
            {RTime, MState1} ->
                {RTime/1000, State#st{state = MState1}}
        end,
    lager:info("~s: heatbeat in ~fms", [Name, MsTime]),
    % zeta:cvh(["heartbeat beat_time"], MsTime),
    {ok, NewState}.

eval_beat_async(State = #st{type = Type}) ->
    Parent = self(),
    erlang:spawn_link(
      fun () ->
              {ok, #st{state = NewMState}} = eval_beat(State),
              Parent ! {update_state, NewMState}
      end),
    ok.

code_change(OldVsn, 
            State = #st{state = MState0, module = Module}, 
            Extra) -> 
    {ok, MState1} = Module:code_change(OldVsn, MState0, Extra),
    {ok, State#st{state = MState1}}.
