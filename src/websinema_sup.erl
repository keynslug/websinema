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

-module(websinema_sup).
-behaviour(supervisor).

-export([init/1]).

%% Callbacks

init([]) ->
    Strategy = {one_for_one, 10, 10},
    ChildSpecs = [
        specify(websinema_aggregator, local, {options, inherit}),
        specify(websinema_frontend, {options, inherit})
    ],
    {ok, {Strategy, ChildSpecs}}.

%% Utilities

specify(Spec = {Name, _, _}, Reg, {options, Mode}) ->
    specify(Spec, Reg, options(Mode, Name));

specify({Name, Module, Entry}, none, Args) ->
    {Name, {Module, Entry, [Args]}, permanent, 5000, worker, [Module]};

specify({Name, Module, Entry}, Tag, Args) ->
    {Name, {Module, Entry, [{Tag, Name}, Args]}, permanent, 5000, worker, [Module]};

specify({Name, Module}, Reg, Args) ->
    specify({Name, Module, start_link}, Reg, Args);

specify(Module, Reg, Args) ->
    specify({Module, Module, start_link}, Reg, Args).

specify(Spec, Args) ->
    specify(Spec, none, Args).

options(inherit, Name) ->
    websinema:option(Name, []);

options(_, _) ->
    [].