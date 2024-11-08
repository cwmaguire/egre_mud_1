%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_quest_ack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner}, Props, {Owner, quest, Self, ack}})
  when Self == self() ->
    Log = [{?EVENT, ack_quest},
           {?SOURCE, Owner},
           {?TARGET, self()}],
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {_, init}}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, init},
           {?TARGET, self()}],
    Owner = proplists:get_value(owner, Props),
    % TODO: time out if we don't get the ack back, meaning we never got
    %       recorded in the character?
    egre:attempt(Owner, {Owner, quest, self()}, false),
    {Props, Log};
succeed({Props, {Owner, quest, _Self, ack}}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, quest_ack},
           {?TARGET, self()}],
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
