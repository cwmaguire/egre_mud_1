%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_counterattack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{}, Props, {Attacker, attack, Self}, _}) when Self == self() ->
    Log = [{?EVENT, attack},
           {?SOURCE, Attacker},
           {?TARGET, Self}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Character, stop_attack}, _}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, stop_attack}],
    Props = lists:keystore(is_attacking, 1, Props, {is_attacking, false}),
    {Props, Log};

succeed({Props, {Attacker, attack, Self}, _}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, attack},
           {?TARGET, Self}],
    case proplists:get_value(is_attacking, Props) of
        true ->
            ok;
        _ ->
            egre_object:attempt(self(), {self(), counter_attack, Attacker})
    end,
    {Props, Log};
succeed({Props, {Self, counter_attack, Target}, _}) ->
    Log = [{?SOURCE, Self},
           {?EVENT, counter_attack},
           {?TARGET, Target}],
    egre_object:attempt(self(), {self(), attack, Target}),
    {Props, Log};
succeed(_) ->
    undefined.

fail({Props, _Result, _Msg, _}) ->
    Log = [{?EVENT, attack},
           {?TARGET, self()}],
    {Props, Log}.
