%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_experience_gain).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner}, Props,
         {Source, killed, Owner, with, _AttackVector}}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Source},
           {?TARGET, Owner}],
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt({#parents{owner = Owner}, Props,
         {Owner, gains, _Exp, experience}}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Owner},
           {?TARGET, self()}],
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Source, killed, Owner, with, _AttackEffect}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, killed},
           {?TARGET, Owner}],
    Exp = proplists:get_value(gives, Props),
    egre:attempt(Source, {Source, gains, Exp, experience}),
    {Props, Log};
succeed({Props, {Owner, gains, NewExp, experience}}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, experience_gain},
           {?TARGET, self()}],
    OldExp = proplists:get_value(gained, Props),
    TotalExp = OldExp + NewExp,
    NewProps = [{gained, TotalExp} | proplists:delete(gained, Props)],
    egre:attempt(Owner, {Owner, has, TotalExp, experience}),
    {NewProps, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
