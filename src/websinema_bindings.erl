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

-module(websinema_bindings).

%% Exports

-export([discover/2, discover/4, examine/3, examine/4, varalias/1, varaliases/1, varname/1, varoid/1]).

%% Public API

-define(NEXT_ANY_OID, [0, 0, 0]).
-define(SUBTREE_TID, 'OBJECT IDENTIFIER').
-define(DISCOVERY_TIMEOUT, 800).

discover(User, Agent) ->
    discover(User, Agent, []).

discover(User, Agent, Options) ->
    try
        {ok, discover({User, Agent}, ?NEXT_ANY_OID, [], Options)}
    catch 
        _:Reason -> {error, {discovery_failed, Reason}}
    end.

examine(User, Agent, Oids) ->
    examine(User, Agent, Oids, 5000).

examine(User, Agent, Oids, Timeout) ->
    case snmpm:sync_get(User, Agent, Oids, Timeout) of
        {ok, Pdu, _}   -> objects(Pdu);
        {error, Error} -> throw(Error)
    end.

varaliases(List) ->
    [ {varalias(ID), Value} || {ID, Value} <- List ].

varalias(ID = [I|_]) when is_integer(I) ->
    List = lists:zip(lists:seq(1, length(ID)), ID),
    PrefixSet = [ [ E1 || {N1, E1} <- List, N1 =< N0 ] || {N0, _} <- List ],
    case varalias(PrefixSet, {[], []}) of
        {Got, [0]}  -> Got;
        {Got, Rest} -> Got ++ Rest
    end;

varalias(Invalid) ->
    throw({invalid_oid, Invalid}).

varoid(L) ->
    varoid(L, []).

varoid(L = [H | _], _) when is_integer(H) ->
    L;

varoid(L = [H | _], Acc) when is_atom(H) ->
    [Last | Rev] = lists:reverse(L),
    case snmpm:name_to_oid(Last) of
        {ok, [Id]} -> Id ++ Acc;
        _Error     -> varoid(lists:reverse(Rev), [Last | Acc])
    end;

varoid(Invalid, _) ->
    throw({invalid_name, Invalid}).

varname(List) when is_list(List) ->
    string:join([ utilities:to_list(E) || E <- List ], ".").

%% Internals

discover(A = {User, Agent}, OID, Tree, Options) ->
    case snmpm:sync_get_next(User, Agent, [OID], ?DISCOVERY_TIMEOUT) of
        {ok, Pdu, _} -> 
            lager:debug("Discovered new object after ~p", [varalias(OID)]),
            case object(Pdu, relax) of
                {error, {object_ignored, NextOID}} -> 
                    discover(A, NextOID, Tree, Options);
                {error, finish} ->
                    Tree;
                {error, _} ->
                    [_, Last | Rest] = lists:reverse(OID),
                    NextOID = lists:reverse([Last + 1 | Rest]),
                    discover(A, NextOID, Tree, Options);
                {NextOID, Object} ->
                    discover(A, NextOID, [{NextOID, Object} | Tree], Options)
            end;
        {error, {timeout, _}} ->
            lager:debug("Discovery after ~p has been timed out, retrying...", [varalias(OID)]),
            discover(A, OID, Tree, Options);
        {error, Error} ->
            throw(Error)
    end.

objects({noError, _, Objects}) when is_list(Objects) ->
    [ Object || Object = {_, Value} <- [ object(O, suicide) || O <- Objects ], Value =/= ignore ];
objects(Error) ->
    throw(Error).

object({noError, _, [Object]}, Failover) ->
    object(Object, Failover);
object({varbind, OID, _, noSuchObject, _}, Failover) ->
    failover(Failover, {no_object, varalias(OID)});
object({varbind, OID, _, noSuchInstance, _}, Failover) ->
    failover(Failover, {no_instance, varalias(OID)});
object({varbind, _, _, endOfMibView, _}, Failover) ->
    failover(Failover, finish);
object({varbind, OID, Type, Value, _}, Failover) ->
    case vartype(Type) of
        ignore     -> failover(Failover, {object_ignored, OID});
        NativeType -> {OID, {NativeType, Value}}
    end;
object(Error, Failover) ->
    failover(Failover, Error).

failover(relax, Error) -> {error, Error};
failover(_, Error) -> throw(Error).

varalias([H|T], Acc) ->
    {L, R} = varalias(T, Acc),
    case snmpm:oid_to_name(H) of
        {ok, Name}      -> {[Name | L], R};
        _ when L =:= [] -> {L, [lists:last(H) | R]};
        _               -> {L, R}
    end;

varalias([], Acc) -> 
    Acc.

vartype('OCTET STRING') -> string;
vartype('OBJECT IDENTIFIER') -> object;
vartype('DisplayString') -> string;
vartype('TimeTicks') -> ticks;
vartype('TimeStamp') -> timestamp;
vartype('INTEGER') -> integer;
vartype('Integer32') -> integer;
vartype('Unsigned32') -> integer;
vartype('Counter32') -> integer;

vartype(_Another) -> ignore.
