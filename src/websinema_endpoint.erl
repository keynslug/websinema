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

-module(websinema_endpoint).
-behaviour(snmpm_user).

%% Exports

-export([handle_agent/5, handle_error/3, handle_inform/3, handle_pdu/4, handle_report/3, handle_trap/3]).

%% Public API

handle_error(ReqId, Reason, Server) ->
    dispatch(Server, {agent, error}, {ReqId, Reason}),
    ignore.

handle_agent(Addr, Port, Type, SnmpInfo, Server) ->
    dispatch(Server, {agent, unknown}, {Addr, Port, Type, SnmpInfo}),
    ignore.

handle_pdu(TargetName, ReqId, SnmpResponse, Server) ->
    dispatch(Server, {agent, pdu}, {TargetName, ReqId, SnmpResponse}),
    ignore.

handle_trap(TargetName, SnmpTrap, Server) ->
    dispatch(Server, {agent, trap}, {TargetName, SnmpTrap}),
    ignore.

handle_inform(TargetName, SnmpInform, Server) ->
    dispatch(Server, {agent, inform}, {TargetName, SnmpInform}),
    ignore.

handle_report(TargetName, SnmpReport, Server) ->
    dispatch(Server, {agent, report}, {TargetName, SnmpReport}),
    ignore.

dispatch(Ref, Tag, Info) ->
    Ref ! {Tag, Info}.
