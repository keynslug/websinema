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

-module(websinema_frontend).
-behaviour(supervisor).
-behaviour(cowboy_http_handler).

-export([start_link/0, start_link/1]).

-export([init/1]).
-export([init/3, handle/2, terminate/2]).

-define(DEFAULTS, [
    {port, 8801},
    {docroot, "client/websinema"}
]).

%% Public API

start_link() ->
    start_link(?DEFAULTS).

start_link(InitialOptions) ->
    lager:info("Initializing websinema server frontend..."),
    
    Options = websinema_utilities:propdefaults(?DEFAULTS, InitialOptions),
    TransportOptions = websinema_utilities:proplist([ip, port, backlog], Options),
    ProtoOptions = websinema_utilities:proplist([timeout], Options),
    
    DocRoot = docroot(websinema_utilities:propget([docroot], Options)),
    DocRootBinary = list_to_binary(DocRoot),
    
    Dispatch = [
        {'_', [
            {[<<"endpoint">>], websinema_ws, {websocket, []}},
            {'_',              ?MODULE, {static, DocRootBinary}}
        ]}
    ],
    
    Transport = cowboy_tcp_transport,
    Proto = cowboy_http_protocol,
    {ok, _} = cowboy:start_listener(http, 8, Transport, TransportOptions, Proto, [{dispatch, Dispatch} | ProtoOptions]),
    
    supervisor:start_link(?MODULE, []).

%% Supervisor callback

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

%% Http handler behaviour

init({_Any, http}, Request, {static, DocRoot}) ->
    [Peer, Method, RawPath] = websinema_utilities:examine_request(Request, [peer, method, raw_path]),
    lager:debug("New ~p request on ~p was received from ~s", [Method, RawPath, websinema_utilities:peername(Peer)]),
    case Method of
        Method when Method =:= 'GET' orelse Method =:= 'HEAD' -> 
            {ok, Request, {DocRoot, RawPath}};
        _Other -> 
            {shutdown, Request, undefined}
    end.

handle(Request, {_, <<"/favicon.ico">>}) ->
    {ok, Request, undefined};

handle(Request, {DocRoot, <<"/">>}) ->
    handle(Request, {DocRoot, <<"/index.html">>});

handle(Request, {DocRoot, RawPath}) ->
    {ok, NewRequest} = case serve_file(DocRoot, RawPath) of
                           {error, Error} ->
                               cowboy_http_req:reply(404, [], websinema_utilities:to_binary(Error), Request);
                           {Format, Data} -> 
                               cowboy_http_req:reply(200, [{'Content-Type', Format}], Data, Request)
                       end,
    {ok, NewRequest, undefined}.

terminate(_Request, _State) ->
    ok.

serve_file(DocRoot, Path) ->
    case file:read_file(<<DocRoot/binary, Path/binary>>) of
        {ok, Binary} -> {extract_format(Path), Binary};
        Error        -> Error
    end.
        
extract_format(Path) ->
    case re:run(Path, "[^.]+$") of
        {match, [Slice]} -> format(binary:part(Path, Slice));
        _                -> format(undefined)
    end.

format(<<"xhtml">>) -> <<"application/xhtml+xml">>;
format(<<"html">>)  -> <<"text/html">>;
format(<<"htm">>)   -> <<"text/html">>;
format(<<"js">>)    -> <<"text/javascript">>;
format(<<"css">>)   -> <<"text/css">>;
format(<<"xml">>)   -> <<"application/xml">>;
format(<<"json">>)  -> <<"application/json">>;
format(_)           -> <<"text/plain">>.

%% Utilities

docroot(DocRoot) ->
    Reversed = lists:reverse(DocRoot),
    case Reversed of
        [$/ | Rest] -> websinema:priv(lists:reverse(Rest));
        [$\ | Rest] -> websinema:priv(lists:reverse(Rest));
        _           -> websinema:priv(DocRoot)
    end.
