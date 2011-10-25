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

-module(websinema_ws).
-behaviour(cowboy_http_websocket_handler).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-export([encode/2]).

-record(ws_state, {
    aggregator,
    interval = 0,
    timer = undefined,
    subscription = []
}).

%% Behavoiur

init({_Any, http}, Request, {websocket, _}) ->
    case cowboy_http_req:header('Upgrade', Request) of
        {undefined, _}       -> {shutdown, Request, undefined};
        {<<"websocket">>, _} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _} -> {upgrade, protocol, cowboy_http_websocket}
    end.

websocket_init(_Any, Request, {websocket, _PassedOptions}) ->
    [Peer, RawPath] = websinema_utilities:examine_request(Request, [peer, raw_path]),
    lager:info("Websocket client at ~p accepted on ~p", [websinema_utilities:peername(Peer), RawPath]),
    State = #ws_state{aggregator = websinema_aggregator},
    {ok, Request, State, hibernate}.

websocket_handle({ping, _}, Request, State) ->
    lager:debug("Websocket instance was poked"),
    {ok, Request, State};

websocket_handle({text, Message}, Request, State) ->
    lager:debug("Websocket instance received a message: ~p", [Message]),
    {RawReply, NewState} =
        case request(Message) of
                {ok, {Action, Object}} ->
                    case catch handle_request(Action, Object, State) of
                        {ok, {ok, Reply}, AState} -> {respond({ok, Reply}), AState};
                        {ok, Reply, AState}       -> {respond({ok, Reply}), AState};
                        {ok, {ok, Reply}}         -> {respond({ok, Reply}), State};
                        {ok, Reply}               -> {respond({ok, Reply}), State};
                        Error                     -> {respond(Error), State}
                    end;
                Error ->
                    {respond(Error), State}
            end,
    {reply, {text, RawReply}, Request, NewState, hibernate};

websocket_handle(Unexpected, Request, State) ->
    lager:error("Websocket instance got an unexpected message from client: ~p", [Unexpected]),
    {ok, Request, State, hibernate}.

websocket_info({timeout, resend}, Request, State = #ws_state{subscription = Subscription, aggregator = Aggregator, interval = Interval}) ->
    Response = do_examine(Aggregator, Subscription, {until, Interval div 1000}, average),
    {reply, {text, respond(Response)}, Request, State, hibernate};

websocket_info(Unexpected, Request, State) ->
    lager:error("Websocket instance received unexpected message: ~p", [Unexpected]),
    {ok, Request, State, hibernate}.

websocket_terminate(Reason, _Request, #ws_state{interval = Interval, subscription = Subscription, aggregator = Aggregator}) ->
    lager:info("Websocket client released because of ~p", [Reason]),
    timer:cancel(Interval),
    do_suspend(Aggregator, Subscription),
    ok.

%% Handlers

handle_request(<<"agents_list">>, _, #ws_state{aggregator = Aggregator}) ->
    List = websinema_aggregator:agents(Aggregator),
    {ok, [ websinema_utilities:to_binary(A) || A <- List ]};

handle_request(<<"discover">>, Args, #ws_state{aggregator = Aggregator}) ->
    Name = proplists:get_value(<<"name">>, Args),
    NameList = binary_to_list(Name),
    case websinema_aggregator:discover(Aggregator, NameList) of
        {ok, Discovery} ->
            {ok, [{name, Name}, {metrics, encode({tree, bindings}, Discovery)}]};
        Error ->
            Error
    end;

handle_request(<<"subscribe">>, Args, State = #ws_state{aggregator = Aggregator, subscription = WasSubscription, interval = WasInterval, timer = WasTimer}) ->
    [Ids, NewInterval] = websinema_utilities:propvalues([<<"sids">>, <<"i">>], Args),
    {Pushed, Polled, Subscription} = lists:foldl(fun alter_subscription/2, {[], [], WasSubscription}, Ids),
    do_resume(Aggregator, Pushed),
    do_suspend(Aggregator, Polled),
    Interval = case empty_subscription(Subscription) of true -> 0; _ -> NewInterval end,
    Timer = alter_timer(WasTimer, WasInterval, Interval, {self(), resend}),
    Response = do_examine(Aggregator, Pushed, every, list),
    {ok, Response, State#ws_state{interval = Interval, subscription = Subscription, timer = Timer}};

handle_request(Request, _, _) ->
    lager:error("Websocket instance received invalid request: ~p", [Request]),
    {error, {invalid_request, Request}}.

%% Transport

encode(T = {tree, _}, Values = [{_, _}|_]) ->
    [ encode(T, E) || E <- Values ];
encode(T = {tree, bindings}, {Prefix, List}) when is_list(List) ->
    {[ {name, encode(oid, Prefix)}, {category, group}, {children, encode(T, List)} ]};
encode(T = {tree, values}, {Name, List}) when is_list(List) ->
    {[ {name, encode(binary, Name)}, {metrics, encode(T, List)} ]};

encode({tree, T}, {Name, Value}) ->
    {[ {sid, Name}, {name, encode(oid, Name)} | encode(T, Value) ]};

encode(bindings, {Type, Value}) ->
    [ {category, metric}, {type, Type}, {value, encode(metric, {Type, Value})} ];

encode(values, {value, Type, L = [{_, _} | _]}) ->
    [ {values, [ {encode(values, {value, Type, E})} || E <- L ]} ];
encode(values, {value, Type, {Ts, Value}}) ->
    [ {ts, Ts}, {value, encode(metric, {Type, Value})} ];
encode(values, Error) ->
    [ {error, do_respond(Error)} ];

encode(metrics, []) ->
    [];
encode(metrics, Metrics) ->
    {[ {tag, metrics}, {value, encode({tree, values}, Metrics)} ]};
    

encode(oid, Name = [_|_]) ->
    lists:foldl(
        fun 
           (E, Acc) when Acc =:= <<>> -> <<(websinema_utilities:to_binary(E))/binary>>;
           (E, Acc) -> <<Acc/binary, ".", (websinema_utilities:to_binary(E))/binary>>
        end, <<>>, Name
    );

encode(metric, {string, Value}) ->
    iolist_to_binary(Value);
encode(metric, {_, Value}) ->
    Value;

encode(binary, Name) ->
    websinema_utilities:to_binary(Name);

encode(_, []) ->
    [].

%% Utilities

request(Request) ->
    case json:decode(Request) of
        {ok, {Object}} ->
            Action = proplists:get_value(<<"a">>, Object),
            {ok, {Action, Object}};
        Error ->
            Error
    end.

respond(Reply) ->
    try 
        %% lager:debug("Reply: ~p", [Reply]),
        {ok, Json} = json:encode(do_respond(Reply)),
        Json
    catch 
        _:_ -> 
            {ok, Error} = json:encode(do_respond({error, malformed_response})),
            Error
    end.

do_respond(Status) when is_atom(Status) -> {[{status, Status =:= ok}]};

do_respond({ok, Data = {_, _}})         -> do_respond({ok, {[Data]}});
do_respond({ok, Data = [{_, _} | _]})   -> do_respond({ok, {Data}});
do_respond({ok, Data})                  -> {[{status, true}, {message, Data}]};

do_respond({_Error, Reason})            -> {[{status, false}, {reason, do_reason(Reason)}]};

do_respond(Arbitrary)                   -> Arbitrary.

do_reason({Why, What}) when is_binary(What) -> do_reason({Why, binary_to_list(What)});
do_reason({Why, What})                      -> iolist_to_binary(io_lib:format("~p: ~p", [Why, What]));
do_reason(Why)                              -> iolist_to_binary(io_lib:format("~p", [Why])).

do_examine(Aggregator, Bindings, Scope, Aggregation) ->
    Metrics = websinema_aggregator:examine(Aggregator, Bindings, Scope, Aggregation, [aliases]),
    case Metrics of
        {ok, Values} -> {ok, encode(metrics, Values)};
        Error        -> Error
    end.

do_resume(Aggregator, Bindings) ->
    websinema_aggregator:resume(Aggregator, Bindings).

do_suspend(Aggregator, Bindings) ->
    websinema_aggregator:suspend(Aggregator, Bindings).

empty_subscription(Subscription) ->
    {_, Lists} = lists:unzip(Subscription),
    lists:append(Lists) =:= [].

alter_subscription({Entry}, Subscription) ->
    [NameBin, IdBin, On] = websinema_utilities:propvalues([<<"a">>, <<"sid">>, <<"on">>], Entry),
    Name = binary_to_list(NameBin),
    Id = decode(oid, IdBin),
    alter_subscription(Name, Id, On, Subscription).

alter_subscription(Name, Id, On, {Pushed0, Polled0, Subscription}) ->
    InitialSubs = lists:delete(Id, websinema_utilities:propget([Name], Subscription, [])),
    {Pushed, Polled, Subs} = case On of
        false -> 
            P = websinema_utilities:propappend([Name], Id, Polled0),
            {Pushed0, P, InitialSubs};
        true  ->
            P = websinema_utilities:propappend([Name], Id, Pushed0),
            {P, Polled0, [Id | InitialSubs]}
    end,
    {Pushed, Polled, websinema_utilities:propset([Name], Subs, Subscription)}.

decode(oid, []) ->
    [];
decode(oid, [Part | Rest]) when is_binary(Part) ->
    [binary_to_atom(Part, utf8) | decode(oid, Rest)];
decode(oid, [Part | Rest])  ->
    [Part | decode(oid, Rest)].

alter_timer(Was, WasInterval, WasInterval, _) ->
    Was;
alter_timer(Was, _, Interval, What) ->
    cancel_timer(Was),
    start_timer(Interval, What).

cancel_timer(undefined) -> ok;
cancel_timer(TimerRef)  -> 
    {ok, cancel} = timer:cancel(TimerRef).

start_timer(0, _)                     -> undefined;
start_timer(Interval, {Pid, Message}) -> 
    {ok, Timer} = timer:send_interval(Interval, Pid, {timeout, Message}), 
    Timer.
