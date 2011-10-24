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

-module(websinema_aggregator).
-behaviour(gen_server).

%% Exports

-export([
    start_link/1, 
    start_link/2,
    stop/1,
    stop/2,
    
    agents/1,
    enlist/3,
    dismiss/2,
    discover/2,
    discover/3,
    examine/3,
    examine/4,
    examine/5
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    user_id,
    agents,
    options
}).

%% Public API

-define(DEFAULT_PRESENTATION, [{tree, preserve}, aliases, {ignore, [[snmpModules]]}]).

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

start_link(Name, Options) ->
    gen_server:start_link(Name, ?MODULE, Options, []).

stop(Name) ->
    stop(Name, shutdown).

stop(Name, Reason) ->
    gen_server:call(Name, {shutdown, Reason}, infinity).

agents(Name) ->
    async(Name, get_agents_list).

enlist(Name, AgentName, AgentOptions) ->
    sync(Name, {enlist_agent, {AgentName, AgentOptions}}).

dismiss(Name, AgentName) ->
    sync(Name, {dismiss_agent, AgentName}).

discover(Name, AgentName) ->
    discover(Name, AgentName, ?DEFAULT_PRESENTATION).

discover(Name, AgentName, Options) ->
    async(Name, {discover_agent, AgentName, Options}).

examine(Name, View, Scope) ->
    examine(Name, View, Scope, average).

examine(Name, View, Scope, Aggregate) ->
    examine(Name, View, Scope, Aggregate, ?DEFAULT_PRESENTATION).

examine(Name, {agent, AgentName}, Scope, Aggregate, Options) ->
    examine(Name, [{AgentName, [[]]}], Scope, Aggregate, Options);

examine(Name, {object, AgentName, ID}, Scope, Aggregate, Options) ->
    examine(Name, [{AgentName, [ID]}], Scope, Aggregate, Options);

examine(Name, View, Scope, Aggregate, Options) ->
    async(Name, {examine_view, View, Scope, Aggregate, Options}).

%% Definitions

-define(DEFAULT_DIR, "manager").
-define(DEFAULT_CONFIG, [{address, [0, 0, 0, 0]}, {port, 5000}, {engine_id, "mgrEngine"}, {max_message_size, 484}]).
-define(DEFAULT_OPTIONS(Dir, V), [
    {mibs, []}, 
    {versions, [v1, v2]}, 
    {server, [{verbosity, V}]},
    {config, [{dir, Dir}, {db_dir, Dir}, {verbosity, V}]},
    {observation, [
        {interval, 1000},
        {backlog, 600}, 
        {rules, [
            {integer, {watch, buffer}},
            {ticks, {watch, actual}},
            {timestamp, {watch, actual}}
        ]}
    ]}
]).

-define(MIBS, [
   {any, ["SNMP-NOTIFICATION-MIB", "SNMP-COMMUNITY-MIB"]},
   {v1,  ["STANDARD-MIB"]},
   {v2,  ["SNMPv2-MIB", "SNMPv2-TM", "SNMP-VIEW-BASED-ACM-MIB"]},
   {v3,  ["SNMP-USM-AES-MIB"]}
]).

%% Callbacks

init(Options) ->
    lager:info("Initializing websinema aggregator..."),
    process_flag(trap_exit, true),
    
    Sites = [manager, mibs, user, agents],
    Starter = fun 
                 (Site, {ok, L}) -> 
                      try start(Site, Options, L) of
                          ok      -> {ok, L};
                          {ok, N} -> {ok, L ++ N};
                          Error   -> throw(Error)
                      catch 
                          _:Reason ->
                              lager:error("Error starting ~p because of ~p", [Site, Reason]),
                              {stop, {init_failed, Site}} 
                      end;
                 (_, Error) -> Error 
              end,
    Result = lists:foldl(Starter, {ok, []}, Sites),
    construct_state(Result).

terminate(Reason, #state{agents = Agents, user_id = User}) ->
    lager:info("Terminating with reason ~p", [Reason]),
    [ finish(agent, A, User) || {A, _} <- Agents ],
    finish(user, User),
    ok.

code_change(_WasVersion, State, _Extra) ->
    {ok, State}.

handle_call({shutdown, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call({sync, Request}, _, State) ->
    {Reply, NewState} = handle_sync_request(Request, State),
    {reply, Reply, NewState};

handle_call({async, Request}, From, State) ->
    spawn(fun () -> Reply = handle_async_request(Request, State), gen_server:reply(From, Reply) end),
    {noreply, State};

handle_call(Request, From, State) ->
    lager:error("Unexpected call ~p received from ~p", [Request, From]),
    {noreply, State}.

handle_cast(Request, State) ->
    lager:error("Unexpected cast ~p received", [Request]),
    {noreply, State}.

handle_info({'EXIT', Observer, Error}, State = #state{agents = InitialAgents}) ->
    Agents = lists:map(
        fun (E = {Name, Props}) ->
            Observers = websinema_utilities:propget([observers], Props),
            case lists:keyfind(Observer, 2, Observers) of
                false -> E;
                {Oid, Observer} ->
                    lager:error("Observer on ~p::~p exited with error ~p", [Name, websinema_bindings:varalias(Oid), Error]),
                    NewProps = websinema_utilities:propset([observers, Oid], not_accessible, Props),
                    {Name, NewProps}
            end
        end,
    InitialAgents),
    {noreply, State#state{agents = Agents}};

handle_info(Message, State) ->
    lager:error("Unexpected message received ~p", [Message]),
    {noreply, State}.

%% Initialization

start(manager, Options, _) ->
    Verbose = proplists:get_value(verbose, Options, silence),
    ManagerDir = websinema:priv(proplists:get_value(manager_dir, Options, ?DEFAULT_DIR)),
    InitialConfig = proplists:get_value(manager_config, Options),
    InitialOptions = proplists:get_value(manager_options, Options, []),
    
    ManagerOptions = websinema_utilities:propdefaults(?DEFAULT_OPTIONS(ManagerDir, Verbose), InitialOptions),
    ManagerConfig = websinema_utilities:propdefaults(?DEFAULT_CONFIG, InitialConfig),

    ok = snmpm_conf:write_manager_config(ManagerDir, ManagerConfig),
    ok = snmpm:start_link(ManagerOptions),
    {ok, [{options, ManagerOptions}]};

start(mibs, Options, State) ->
    Fixed = proplists:get_value(mibs, Options, []),
    Versions = proplists:get_value(versions, proplists:get_value(options, State), []),
    Relevant = [ E || {any, L} <- ?MIBS, E <- L ] ++ [ E || {V, L} <- ?MIBS, E <- L, V0 <- Versions, V =:= V0 ],
    Files = expand_mib_filenames(local, Fixed) ++ expand_mib_filenames(global, Relevant),
    Result = [ {E, snmpm:load_mib(E)} || E <- Files ],
    Loaded = [ E || {E, ok} <- Result ],
    [ lager:error("Mib binary ~p failed to load properly because of ~p", [E, Reason]) || {E, {error, Reason}} <- Result ],
    {ok, [{mibs, Loaded}]};

start(user, Options, _) ->
    UserID = proplists:get_value(user_id, Options, ?MODULE),
    Endpoint = proplists:get_value(endpoint, Options, websinema_endpoint),
    ok = snmpm:register_user_monitor(UserID, Endpoint, self()),
    {ok, [{user_id, UserID}]};

start(agents, Options, State) ->
    UserID = proplists:get_value(user_id, Options, ?MODULE),
    Agents = proplists:get_value(agents, Options, []),
    Ready = lists:foldl(
        fun (Agent = {Name, Opts}, Ready) ->
                 case catch start(agent, {Agent, UserID}, State) of
                     {ok, Discovery, Observers} ->
                         Props = {Name, [ {remote, Opts}, {bindings, Discovery}, {observers, Observers} ]},
                         [Props | Ready];
                     Error ->
                       lager:error("The agent ~p has not been registered because of ~p", [Agent, Error])
                 end
        end,
        [], Agents
    ),
    {ok, [{agents, Ready}]};
    
start(agent, {Agent, User}, State) ->
    {Name, Opts} = Agent,
    ok = snmpm:register_agent(User, Name, config(agent, Name, Opts)),
    case websinema_bindings:discover(User, Name) of
        {ok, Bindings} ->
            Observers = start(observers, {Name, User, Bindings}, State),
            {ok, Bindings, Observers};
        Error -> 
            throw(Error)
    end;

start(observers, {Name, User, Bindings}, State) ->
    ObserveOptions = [
        {agent, Name}, {user, User} | 
        websinema_utilities:propget([options, observation], State)
    ],
    [ {Oid, websinema_observer:start_link(Oid, Value, ObserveOptions)} || {Oid, Value} <- Bindings ].

finish(agent, Name, User) ->
    catch snmpm:unregister_agent(User, Name).

finish(user, User) ->
    catch snmpm:unregister_user(User).

%% Calls dispatching

sync(Name, Request)  -> gen_server:call(Name, {sync, Request}).
async(Name, Request) -> gen_server:call(Name, {async, Request}).

handle_sync_request(Request, State) ->
    try request(Request, State) of
        {ok, Result, NewState} -> {Result, NewState};
        Error = {error, _}     -> {Error, State};
        AnotherError           -> {{error, AnotherError}, State}
    catch _:Reason -> {{error, Reason}, State}
    end.

handle_async_request(Request, State) ->
    try request(Request, State) of
        {ok, Result} -> Result;
        Error        -> {error, Error}
    catch _:Reason   -> {error, Reason}
    end.

%% Operations

request(get_agents_list, #state{agents = Agents}) ->
    {ok, [ Name || {Name, _} <- Agents ]};

request({has_agent, Name}, #state{agents = Agents}) ->
    {ok, proplists:is_defined(Name, Agents)};

request({enlist_agent, Agent = {Name, Remote}}, State = #state{agents = Agents, user_id = User}) ->
    assert_no_agent(Name, Agents),
    case start(agent, {Agent, User}, none) of
        ok ->
            NewAgents = websinema_utilities:propset([Name, endpoint], Remote, Agents),
            {ok, ok, State#state{agents = NewAgents}};
        Error ->
            Error
    end;

request({dismiss_agent, Name}, State = #state{agents = Agents, user_id = User}) ->
    assert_agent(Name, Agents),
    Result = finish(agent, Name, User),
    NewAgents = proplists:delete(Name, Agents),
    {ok, Result, State#state{agents = NewAgents}};

request({discover_agent, Name, Options}, #state{agents = Agents}) ->
    Agent = assert_agent(Name, Agents),
    Bindings = proplists:get_value(bindings, Agent, undefined),
    {ok, {ok, xform_view(Bindings, Options)}};

request({examine_view, View, Scope, Aggregate, Options}, #state{agents = Agents}) ->
    Ignored = websinema_utilities:propget([ignore], Options, []),
    Expanded = expand_view(View, Ignored, Agents, []),
    Examiner = fun ({Name, Ids}) ->
                        Observers = websinema_utilities:propget([Name, observers], Agents),
                        Bindings = websinema_utilities:propget([Name, bindings], Agents),
                        Values = lists:map(fun (Id) -> {Id, examine_one(Id, Scope, Aggregate, Bindings, Observers)} end, Ids),
                        {Name, xform_view(Values, Options)}
               end,
    {ok, {ok, lists:map(Examiner, Expanded)}};

%% Debug purpose

request(state, State) ->
    {ok, State}.

%% Utilities

config(agent, Name, {Target, Port}) ->
    config(agent, Name, {Target, Port, "public"});

config(agent, Name, {Target, Port, Community}) ->
    [{engine_id, Name}, {version, v2}, {address, Target}, {port, Port}, {community, Community}].

construct_state({ok, Options}) ->
    {ok, #state{
        agents  = proplists:get_value(agents, Options, []),
        user_id = proplists:get_value(user_id, Options)
    }};

construct_state(Another) ->
    Another.

assert_no_agent(Name, Agents) ->
     [ throw({agent_exists, Name}) || proplists:is_defined(Name, Agents) ].

assert_agent(Name, Agents) ->
    case proplists:lookup(Name, Agents) of
        none       -> throw({no_agent, Name});
        {_, Entry} -> Entry
    end.

xform_view(List, Options) ->
    OrderedOpts = [aliases, ignore, tree],
    lists:foldl(fun (Option, L) -> do_xform_view(proplists:lookup(Option, Options), L) end, List, OrderedOpts).

do_xform_view({tree, true}, List) ->
    websinema_utilities:prefix_tree(List);

do_xform_view({tree, preserve}, List) ->
    websinema_utilities:prefix_tree(List, [preserve]);

do_xform_view({aliases, true}, List) ->
    websinema_bindings:varaliases(List);

do_xform_view({ignore, Ignored}, List) ->
    Filter = fun (Ignore, L) -> 
                      FilterOut = websinema_utilities:with_prefix(Ignore, List),
                      lists:filter(fun ({Key, _}) -> not proplists:is_defined(Key, FilterOut) end, L)
             end,
    lists:foldl(Filter, List, Ignored);

do_xform_view(_, List) ->
    List.

expand_view([{any, Ids} | Rest], Ignored, Agents, Acc) ->
    Expanded = [ {N, Ids} || {N, _} <- Agents ],
    expand_view(Expanded ++ Rest, Ignored, Agents, Acc);

expand_view([{Name, Ids} | Rest], Ignored, Agents, Acc) ->
    Agent = assert_agent(Name, Agents),
    Bindings = assert_bindings(Name, Agent),
    expand_view(Rest, Ignored, Agents, [{Name, expand_ids(Ids, Bindings, Ignored)} | Acc]);

expand_view([], _, _, Acc) ->
    Acc.

expand_ids(Ids, Bindings, Ignored) ->
    IgnoreList = do_expand_ids(Ignored, Bindings),
    [ Id || Id <- do_expand_ids(Ids, Bindings), not lists:member(Id, IgnoreList) ].

do_expand_ids([[]], Bindings) ->
    [ Id || {Id, _} <- Bindings ];

do_expand_ids(Ids, Bindings) ->
    Expanded = [ Id || Prefix <- Ids, {Id, _} <- 
        begin 
            Oid = websinema_bindings:varoid(Prefix),
            With = websinema_utilities:with_prefix(Oid, Bindings),
            case With of
                [] -> throw({no_objects, Prefix});
                _  -> With
            end
        end 
    ],
    lists:usort(Expanded).

assert_bindings(Name, Agent) ->
    case proplists:get_value(bindings, Agent) of
        undefined -> throw({no_agent_discovery, Name});
        Bindings  -> Bindings
    end.

expand_mib_filenames(Where, Mibs) ->
    Path = expand_mib_path(Where),
    [ Path ++ "/mibs/" ++ E ++ ".bin" || E <- Mibs ].

expand_mib_path(local)  -> websinema:priv();
expand_mib_path(global) -> code:priv_dir(snmp).

examine_one(Id, Scope, Aggregation, Bindings, Observers) ->
    case websinema_utilities:propget([Id], Observers) of
        Pid when is_pid(Pid) ->
            {Type, _} = websinema_utilities:propget([Id], Bindings),
            {value, Type, websinema_observer:metrics(Pid, Scope, Aggregation)};
        Error ->
            Error
    end.
