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

-module(websinema).

-include_lib("kernel/include/file.hrl").

-export([start/0, stop/0]).
-export([priv/0, priv/1, option/1, option/2]).

start() ->
    application:load(?MODULE),
    ensure_deps_started(),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            {ok, #file_info{type=directory}} = file:read_file_info([priv]),
            "./priv/";
        Value ->
            Value ++ "/"
     end.

priv([$/ | Rest]) ->
    priv() ++ Rest;

priv(Sub) ->
    priv() ++ Sub.

option(Path) ->
    option(Path, undefined).

option(Path, Default) when is_list(Path) ->
    websinema_utilities:propget(Path, application:get_all_env(), Default);

option(Atom, Default) ->
    option([Atom], Default).

ensure_deps_started() ->
    {ok, DepsList} = application:get_key(?MODULE, applications),
    [application:start(App) || App <- DepsList],
    ok.
