%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_attack).
-behaviour(egre_rules).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{}, Props, {Attacker, attack, Self}, _}) when Self == self() ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, attack},
           {?TARGET, Self}],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Self, attack, Target, with, AttackType, _Times, times}, _}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, attack},
           {?TARGET, Target},
           {attack_type, AttackType}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Self, attack, Target, with, AttackType, _Times, times}, _}) ->
    Log = [{?SOURCE, Self},
           {?EVENT, attack},
           {?TARGET, Target},
           {vector, AttackType}],
    Props2 = lists:keystore(is_attacking, 1, Props, {is_attacking, true}),
    {Props2, Log};
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
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
