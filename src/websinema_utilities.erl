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

-module(websinema_utilities).

-export([
    to_list/1,
    to_binary/1,

    propget/2,
    propget/3,
    propset/3,
    propappend/3,
    propextract/2,
    proplist/2,
    propvalues/2,
    propdefaults/2,
    
    prefix_tree/1,
    prefix_tree/2,

    with_prefix/2,
    without_prefix/2,
    
    examine_request/2,
    peername/1
]).

-export([test/0]).

%% Routines

to_binary(X) when is_binary(X)  -> X;
to_binary(X) -> list_to_binary(to_list(X)).

to_list(A) when is_list(A)      -> A;
to_list(A) when is_atom(A)      -> atom_to_list(A);
to_list(A) when is_integer(A)   -> integer_to_list(A);
to_list(A) when is_float(A)     -> float_to_list(A);
to_list(A) when is_binary(A)    -> binary_to_list(A).

%% Prefix tree construction

prefix_tree(List) ->
    prefix_tree(List, []).

prefix_tree(List, Options) ->
    Result = do_prefix_tree(List),
    lists:foldl(fun process_prefix_tree/2, Result, Options).

do_prefix_tree([With = {[], _} | Without]) ->
     [With | do_prefix_tree(Without)];

do_prefix_tree([E | Rest]) ->
     {[First | Last], Value} = E,
     With = [ {Last, Value} | [ {L, V} || {[F | L], V} <- Rest, F =:= First ] ],
     Without = [ E1 || E1 = {[F | _], _} <- Rest, F =/= First ],
     do_partition(First, do_prefix_tree(With), do_prefix_tree(Without));

do_prefix_tree(Any) ->
    Any.

do_partition(First, [{With, Value}], Without) ->
    [{[First | With], Value} | Without];
    
do_partition(First, With, Without) ->
    [{[First], With} | Without].

process_prefix_tree(preserve, List) ->
    preserve_prefix(List, []);

process_prefix_tree({preserve, true}, List) ->
    preserve_prefix(List, []);

process_prefix_tree({transform, Xform}, List) ->
    transform_prefix(List, Xform);

process_prefix_tree(_, List) ->
    List.

preserve_prefix(List = [{_, _} | _], Prefix) ->
    [ {Prefix ++ Pre, preserve_prefix(Value, Prefix ++ Pre)} || {Pre, Value} <- List ];

preserve_prefix(Value, _) ->
    Value.

transform_prefix(List = [{_, _} | _], Xform) ->
    [ {Xform(Prefix), transform_prefix(Value, Xform)} || {Prefix, Value} <- List ];

transform_prefix(Value, _) ->
    Value.

%% Properties deep access

propget(Path, Proplist) ->
    propget(Path, Proplist, undefined).

propget([], Value, _) ->
    Value;
propget([Key | _], [Key | _], Default) ->
    Default;
propget([Key | Rest], [{Key, Value} | _], Default) ->
    propget(Rest, Value, Default);
propget(Path, [_ | Left], Default) ->
    propget(Path, Left, Default);
propget(_, _, Default) ->
    Default.

propset([], Entry, _) -> 
    Entry;
propset([Key | Rest], Entry, Proplist = [{_, _} | _]) ->
    With = [ {K, propset(Rest, Entry, V)} || P = {K, V} <- Proplist, keymatch(Key, P) ],
    Without = [ P || P <- Proplist, not keymatch(Key, P) ],
    case With of
        [E|_] -> [E | Without];
        []    -> [{Key, propset(Rest, Entry, [])} | Without]
    end;
propset([Key | Rest], Entry, Value) ->
    [{Key, propset(Rest, Entry, Value)}].

propappend([], Entry, Acc) -> 
    [Entry | Acc];
propappend([Key | Rest], Entry, Proplist = [{_, _} | _]) ->
    With = [ {K, propappend(Rest, Entry, V)} || P = {K, V} <- Proplist, keymatch(Key, P) ],
    Without = [ P || P <- Proplist, not keymatch(Key, P) ],
    case With of
        [E|_] -> [E | Without];
        []    -> [{Key, propappend(Rest, Entry, [])} | Without]
    end;
propappend([Key | Rest], Entry, Value) ->
    [{Key, propappend(Rest, Entry, Value)}].

propextract(Key, Proplist) ->
    propextract(Key, Proplist, []).

propextract(Key, [Key | Rest], Acc) ->
    {{Key, true}, Acc ++ Rest};
propextract(Key, [E = {Key, _} | Rest], Acc) ->
    {E, Acc ++ Rest};
propextract(Key, [H | Rest], Acc) ->
    propextract(Key, Rest, [H | Acc]).

keymatch(Key, Key) -> true;
keymatch(Key, {Key, _}) -> true;
keymatch(_, _) -> false.

%% Properties multiple access

propvalues(Keys, Proplist) ->
    [ propget(keynormalize(Key), Proplist) || Key <- Keys ].

proplist(Keys, Proplist) ->
    proplist(Keys, Proplist, []).

proplist([Key | Rest], Proplist, Acc) ->
    Norm = keynormalize(Key),
    case propget(Norm, Proplist) of
        undefined -> proplist(Rest, Proplist, Acc);
        Value     -> proplist(Rest, Proplist, [{Key, Value} | Acc])
    end;

proplist([], _, Acc) ->
    Acc.

keynormalize(Key) when is_list(Key) -> Key;
keynormalize(Key) -> [Key].

propdefaults([E = {Head, _} | Defaults], Proplist) ->
    case proplists:is_defined(Head, Proplist) of
        true -> propdefaults(Defaults, Proplist);
        _    -> propdefaults(Defaults, [E | Proplist])
    end;

propdefaults([Head | Defaults], Proplist) ->
    propdefaults([{Head, true} | Defaults], Proplist);

propdefaults([], Proplist) ->
    Proplist.

%% Has prefix

with_prefix(Prefix, Proplist) ->
    has_prefix(true, Prefix, Proplist, []).

without_prefix(Prefix, Proplist) ->
    has_prefix(false, Prefix, Proplist, []).

has_prefix(Match, Prefix, [Entry | Rest], Acc) ->
    case prefixmatch(Prefix, Entry) of
        Match -> has_prefix(Match, Prefix, Rest, [Entry | Acc]);
        _     -> has_prefix(Match, Prefix, Rest, Acc)
    end;

has_prefix(_, _, [], Acc) ->
    Acc.

prefixmatch(Prefix, {Key, _}) -> 
    prefixmatch(Prefix, Key);
prefixmatch(Prefix, Key) ->
    lists:prefix(Prefix, Key).

%% Web

examine_request(Request, What) ->
    [ begin {Value, _} = cowboy_http_req:Ask(Request), Value end || Ask <- What ].

peername({Ip, Port}) ->
    io_lib:format("~s:~p", [peername(Ip), Port]);
peername({A, B, C, D}) ->
    io_lib:format("~B.~B.~B.~B", [A, B, C, D]);
peername({A, B, C, D, E, F, G, H}) ->
    io_lib:format("[~16B:~16B:~16B:~16B:~16B:~16B:~16B:~16B]", [A, B, C, D, E, F, G, H]);
peername(_) ->
    "undefined".

%% Test cases

-include_lib("eunit/include/eunit.hrl").

test() ->
    TestsCount = 14,
    [ ok = test(X) || X <- lists:seq(1, TestsCount) ],
    ok.

test(1) ->
    L = [ {"something", 1}, {"somewhere", 2} ],
    R = [ {"some", [ {"thing", 1}, {"where", 2} ]} ],
    ?assertEqual(R, prefix_tree(L));

test(2) ->
    L = [ {"some", 1}, {"something", 2}, {"somewhere", 3}, {"stupid", 4}, {"there", 5} ],
    R = [ {"s", [ {"ome", [ {[], 1}, {"thing", 2}, {"where", 3} ]}, {"tupid", 4} ]}, {"there", 5} ],
    ?assertEqual(R, prefix_tree(L));

test(9) ->
    L = [ {"some", 1}, {"something", 2}, {"somewhere", 3}, {"stupid", 4}, {"there", 5} ],
    R = [ {"s", [ {"some", [ {"some", 1}, {"something", 2}, {"somewhere", 3} ]}, {"stupid", 4} ]}, {"there", 5} ],
    ?assertEqual(R, prefix_tree(L, [preserve]));

test(10) ->
    L = [ {"some", 1}, {"something", 2}, {"somewhere", 3}, {"stupid", 4}, {"there", 5} ],
    R = [ {"s", [ {"emos", [ {"emos", 1}, {"gnihtemos", 2}, {"erehwemos", 3} ]}, {"diputs", 4} ]}, {"ereht", 5} ],
    ?assertEqual(R, prefix_tree(L, [preserve, {transform, fun lists:reverse/1}]));

test(3) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, [ {level, [ {thing, new}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(R, propset([top, level, thing], new, L));

test(5) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, [ {level, [ {thing2, [{thing3, new}]}, {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(R, propset([top, level, thing2, thing3], new, L));

test(6) ->
    L = [ {top, [ {level, [ {thing2, [{thing3, new}]}, {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [{thing3, new}],
    ?assertEqual(R, propget([top, level, thing2], L));

test(7) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = 4,
    ?assertEqual(R, propget([top, middle], L));

test(14) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, [3]} ]}, {middle, [4]} ]}, {last, 5} ],
    R = [ {top, [ {middle, [new, 4]}, {level, [ {thing, 2}, {where, [3]} ]} ]}, {last, 5} ],
    ?assertEqual(R, propappend([top, middle], new, L));

test(8) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = undefined,
    ?assertEqual(R, propget([top, middle, further], L));

test(4) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]} ]}, {last, 5} ],
    R = new,
    ?assertEqual(R, propset([], new, L));

test(11) ->
    L = [ {level, 1}, {thing, 2}, {where, 3}, {last, 5} ],
    R = { {where, 3}, [ {thing, 2}, {level, 1}, {last, 5} ] },
    ?assertEqual(R, propextract(where, L));

test(12) ->
    L = [ {"level", 1}, {"something", 2}, {"somewhere", 3}, {"last", 5} ],
    R = [ {"somewhere", 3}, {"something", 2} ],
    ?assertEqual(R, with_prefix("some", L));

test(13) ->
    L = [ {level, 1}, {something, 2}, {somewhere, 3}, {last, 5} ],
    R = [ {someone, there}, {level, 1}, {something, 2}, {somewhere, 3}, {last, 5} ],
    Defs = [ {somewhere, none}, {someone, there} ],
    ?assertEqual(R, propdefaults(Defs, L)).
