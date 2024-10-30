%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_level_gain).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner}, Props,
         {Owner, has, _Exp, experience}}) ->
    Log = [{?EVENT, has_experience},
           {?SOURCE, Owner},
           {?TARGET, Owner}],
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt({#parents{owner = Owner}, Props,
         {Owner, level, _Level}}) ->
    Log = [{?EVENT, has_experience},
           {?SOURCE, Owner},
           {?TARGET, Owner}],
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Owner, has, Exp, experience}}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, has_experience},
           {?TARGET, Owner}],
    Level = proplists:get_value(level, Props),
    Levels = proplists:get_value(levels, Props),
    {_, _, _, NewLevel} = lists:foldl(fun notify_levels/2, {Owner, Level, Exp, Level}, lists:sort(Levels)),
    NewProps = [{level, NewLevel} | proplists:delete(level, Props)],
    {NewProps, Log};
succeed({Props, {Owner, level, Level}}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, levelled},
           {?TARGET, Owner}],
    egre:attempt(Owner, {send, Owner, <<"You have reached level ",
                                        (integer_to_binary(Level))/binary>>}),
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

notify_levels({Level, ExpRequired},
              {Owner, CurrLevel, CurrExp, MaxLevel}) when Level > CurrLevel, ExpRequired =< CurrExp ->
  egre:attempt(Owner, {Owner, level, Level}),
  NewMaxLevel = max(Level, MaxLevel),
  {Owner, CurrLevel, CurrExp, NewMaxLevel};
notify_levels(_, Acc) ->
  Acc.
