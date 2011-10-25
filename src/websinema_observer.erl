%% Copyright (c) 2011 Drimmi, Inc.
%% All rights reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(websinema_observer).
-behaviour(gen_server).

-export([start_link/3, stop/1, metrics/3, suspend/1, resume/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    user,
    agent,
    oid,
    type,
    rule,
    interval,
    backlog,
    buffer,
    timer,
    suspension = false,
    failover = false
}).

%% Public API

start_link(Oid, Value, Options) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Oid, Value, Options}, []),
    Pid.

stop(Pid) ->
    gen_server:call(Pid, {shutdown, normal}, infinity).

suspend(Pid) ->
    gen_server:cast(Pid, suspend).

resume(Pid) ->
    gen_server:cast(Pid, resume).

metrics(Pid, Scope, Aggregation) ->
    gen_server:call(Pid, {metrics, Scope, Aggregation}).

%% Callbacks

init({Oid, {Type, Value}, Options}) ->
    lager:info("Initializing new websinema observer on ~p...", [websinema_bindings:varalias(Oid)]),
    process_flag(trap_exit, true),
    
    [User, Agent, Rules, Interval, Backlog, Suspend, Failover] = 
        websinema_utilities:propvalues([user, agent, rules, interval, backlog, suspended, failover], Options),
    Rule = websinema_utilities:propget([Type], Rules, {poll, actual}),
    State = #state{
        user = User,
        agent = Agent, 
        oid = Oid,
        type = Type,
        rule = Rule,
        interval = Interval,
        backlog = Backlog,
        suspension = case Suspend of true -> 0; _ -> none end,
        failover = (Failover =:= true),
        buffer = [do_store(Value)]
    },
    
    Timer = do_watchtime(self(), State),
    {ok, State#state{timer = Timer}, hibernate}.

terminate(Reason, #state{timer = Timer}) ->
    lager:info("Shutting down (~p) a websinema observer...", [Reason]),
    timer:cancel(Timer),
    ok.

code_change(_WasVersion, State, _Extra) ->
    {ok, State, hibernate}.

handle_call({shutdown, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call({metrics, Scope, Agg}, _, State = #state{buffer = Buffer, type = Type, failover = Failover}) ->
    case fetch(Buffer, Scope, Agg) of
        Error = {error, _} ->
            case Failover of
                true  -> {reply, Error, State, hibernate};
                false -> {stop, Error, Error, State}
            end;
        Value ->
            {reply, {value, Type, Value}, State, hibernate}
    end;

handle_call(Request, From, State) ->
    lager:error("Unexpected call ~p received from ~p", [Request, From]),
    {noreply, State, hibernate}.

handle_cast(suspend, State = #state{suspension = 0, oid = Oid}) ->
    lager:debug("Observer on ~p already suspended", [Oid]),
    {noreply, State, hibernate};

handle_cast(suspend, State = #state{suspension = Refs, oid = Oid}) when is_integer(Refs) ->
    lager:debug("Suspending observer on ~p ...", [Oid]),
    {noreply, State#state{suspension = Refs - 1}, hibernate};

handle_cast(suspend, State) ->
    {noreply, State, hibernate};

handle_cast(resume, State = #state{suspension = Refs, oid = Oid}) when is_integer(Refs) ->
    lager:debug("Reactivating observer on ~p ...", [Oid]),
    {noreply, State#state{suspension = Refs + 1}, hibernate};

handle_cast(resume, State) ->
    {noreply, State, hibernate};

handle_cast(Request, State) ->
    lager:error("Unexpected cast ~p received", [Request]),
    {noreply, State, hibernate}.

handle_info(watchtime, State = #state{suspension = 0}) ->
    {noreply, State, hibernate};

handle_info(watchtime, State = #state{oid = Oid, user = User, agent = Agent, interval = Interval}) ->
    Self = self(),
    Ts = timestamp(),
    spawn(fun () -> Self ! {polled, Ts, do_observe(User, Agent, Oid, Interval)} end),
    {noreply, State, hibernate};

handle_info({polled, Ts, What}, State = #state{buffer = Buffer, oid = Oid}) ->
    FreshBuffer = 
        case What of
            {ok, Value} ->
                do_store({Ts, Value}, Buffer, State);
            Error ->
                lager:error("Error arised while observing ~p: ~p", [websinema_bindings:varalias(Oid), Error]),
                {error, not_accessible}
        end,
    {noreply, State#state{buffer = FreshBuffer}, hibernate};

handle_info(Message, State) ->
    lager:error("Unexpected message received ~p", [Message]),
    {noreply, State, hibernate}.

%% Implementation

do_observe(User, Agent, Oid, Interval) ->
    case catch websinema_bindings:examine(User, Agent, [Oid], Interval div 2) of
        [{Oid, {_, Value}}] ->
            {ok, Value};
        [] ->
            {error, not_accessible};
        Error ->
            Error
    end.

do_watchtime(Pid, #state{interval = Interval, rule = {watch, _}}) ->
    timer:send_interval(Interval, Pid, watchtime);
do_watchtime(_, _) ->
    undefined.

do_store(Value, Buffer, State) when not is_list(Buffer) ->
    do_store(Value, [], State);
do_store(Value, Buffer, #state{backlog = Backlog, rule = {_, buffer}}) ->
    [do_store(Value) | lists:sublist(Buffer, Backlog - 1)];
do_store(Value, _, _) ->
    [do_store(Value)].

do_store(Value = {_, _}) ->
    Value;
do_store(Value) ->
    {timestamp(), Value}.

timestamp() ->
    {Megas, Secs, _} = erlang:now(),
    Megas * 1000000 + Secs.

fetch(Error = {error, _}, _, _) ->
    Error;

fetch([Value | _], last, _) ->
    Value;

fetch(List, every, Agg) ->
    aggregate(Agg, List);

fetch(List, {last, N}, Agg) ->
    fetch(lists:sublist(List, N), every, Agg);

fetch(List = [{Ts, _} | _], {until, Seconds}, Agg) ->
    fetch(List, {since, Ts - Seconds + 1}, Agg);

fetch(List, {since, When}, Agg) ->
    fetch(lists:takewhile(fun ({Ts, _}) -> Ts >= When end, List), every, Agg);

fetch(List, _, Agg) ->
    fetch(List, last, Agg).

aggregate(sum, List) ->
    aggregate(sum, List, initial);
aggregate(product, List) ->
    aggregate(product, List, initial);
aggregate(average, List) ->
    {Ts, Sum} = aggregate(sum, List, initial),
    {Ts, operate(average, Sum, length(List))};
aggregate(_, List) ->
    List.

aggregate(Op, [E | Rest], initial) ->
    aggregate(Op, Rest, E);
aggregate(Op, [{_, Value} | Rest], {Ts, Intermediate}) ->
    aggregate(Op, Rest, {Ts, operate(Op, Value, Intermediate)});
aggregate(_, [], Result) ->
    Result.

operate(sum, A, B) when is_integer(A), is_integer(B) -> A + B;
operate(product, A, B) when is_integer(A), is_integer(B) -> A * B;
operate(average, A, B) when is_integer(A), is_integer(B) -> A / B;
operate(_, A, _) -> A.
