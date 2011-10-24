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

-export([discover/2, examine/3, varalias/1, varaliases/1, varname/1, varoid/1]).

%% Public API

-define(NEXT_ANY_OID, [0, 0, 0]).
-define(SUBTREE_TID, 'OBJECT IDENTIFIER').

discover(User, Agent) ->
    discover(User, Agent, []).

discover(User, Agent, Options) ->
    try
        {ok, discover({User, Agent}, ?NEXT_ANY_OID, [], Options)}
    catch 
        _:Reason -> {error, {discovery_failed, Reason}}
    end.

examine(User, Agent, Oids) ->
    case snmpm:sync_get(User, Agent, Oids) of
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

varoid(L = [H | _]) when is_integer(H) ->
    L;

varoid(L = [H | _]) when is_atom(H) ->
    case snmpm:name_to_oid(lists:last(L)) of
        {ok, [Id]} -> Id;
        _Error     -> L
    end;

varoid(Invalid) ->
    throw({invalid_name, Invalid}).

varname(List) when is_list(List) ->
    string:join([ utilities:to_list(E) || E <- List ], ".").

%% Internals

discover(A = {User, Agent}, OID, Tree, Options) ->
    case snmpm:sync_get_next(User, Agent, [OID]) of
        {ok, Pdu, _} -> 
            case object(Pdu) of
                {NextOID, ignore} -> 
                    discover(A, NextOID, Tree, Options);
                {NextOID, Object} ->
                    discover(A, NextOID, [{NextOID, Object} | Tree], Options);
                _ ->
                    Tree
            end;
        Error     -> throw(Error)
    end.

objects({noError, _, Objects}) when is_list(Objects) ->
    [ Object || Object = {_, Value} <- [ object(O) || O <- Objects ], Value =/= ignore ];
objects(Error) ->
    throw(Error).

object({noError, _, [Object]}) ->
    object(Object);
object({varbind, OID, _, noSuchObject, _}) ->
    throw({no_object, varalias(OID)});
object({varbind, OID, _, noSuchInstance, _}) ->
    throw({no_instance, varalias(OID)});
object({varbind, _, _, endOfMibView, _}) ->
    finish;
object({varbind, OID, Type, Value, _}) ->
    case vartype(Type) of
        ignore     -> {OID, ignore};
        NativeType -> {OID, {NativeType, Value}}
    end;
object(Error) ->
    throw(Error).

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
vartype('Counter32') -> integer;

vartype(_Another) -> ignore.
