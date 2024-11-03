%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_search).

-behaviour(egre_rules).

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

attempt({#{}, Props, {search, Src, _Hierarchy}}) ->
    NewMessage = {{resend, Src, _NewMessage = dunno_yet}, _ShouldSubscribe = false, Props},
    {succeed, NewMessage, true, Props}.

succeed({Props, _Msg}) ->
    Props.

fail({Props, _Reason, _Message}) ->
    Props.

%log(Terms) ->
    %mud_event_log:log(debug, [?MODULE | Terms]).
