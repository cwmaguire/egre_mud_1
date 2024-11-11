%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_search).

-behaviour(egre_rules).

-include("mud.hrl").

%% object behaviour
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

id(_Props, Owner, Pid) ->
    "search_for" ++ Owner ++ "_" ++ Pid.

added(_, _) -> ok.
removed(_, _) -> ok.

attempt({#{}, Props, {search, Src, _Hierarchy}, _}) ->
    Log = [{?SOURCE, unknown},
           {?EVENT, search},
           {?TARGET, unknown}],
    NewEvent = {{resend, Src, _NewEvent = dunno_yet}, _ShouldSubscribe = false, Props},
    ?SUCCEED_SUB_NEW_EVENT(NewEvent);
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

%log(Terms) ->
    %mud_event_log:log(debug, [?MODULE | Terms]).
