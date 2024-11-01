%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_quest_kill_five_rats_with_bare_hands).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner}, Props, {Owner, quest, Self, ack}})
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
    egre:attempt(Owner, {Owner, quest, self()}, false),
    {Props, Log};
succeed({Props, {Owner, achievement, _Self, ack}}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, achievement_ack},
           {?TARGET, self()}],
    case proplists:get_value(allow_previous, Props) of
        true ->
          Owner = proplists:get_value(owner, Props),
          egre:attempt(Owner, {Owner, metrics, get, trees_chopped}, false);
        _ ->
          ok
    end,
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
