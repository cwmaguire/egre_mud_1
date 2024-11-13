%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_experience_gain).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner},
         Props,
         {Source, killed, Owner, with, _AttackVector},
         _}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Source},
           {?TARGET, Owner}],
    ?SUCCEED_SUB;
attempt({#{owner := Owner}, Props,
         {Owner, gains, _Exp, experience},
         _}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Owner},
           {?TARGET, self()}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Source, killed, Owner, with, _AttackEffect}, _}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, killed},
           {?TARGET, Owner}],
    Exp = proplists:get_value(gives, Props),
    egre:attempt(Source, {Source, gains, Exp, experience}),
    {Props, Log};
succeed({Props, {Owner, gains, NewExp, experience}, _}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, experience_gain},
           {?TARGET, self()}],
    OldExp = proplists:get_value(gained, Props),
    TotalExp = OldExp + NewExp,
    NewProps = lists:keystore(gained, 1, Props, {gained, TotalExp}),
    egre:attempt(Owner, {Owner, has, TotalExp, experience}),
    {NewProps, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
