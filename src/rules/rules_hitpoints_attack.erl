%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_hitpoints_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{owner := Owner,
           hitpoints := HP},
         Props,
         {Attacker, cause, Amount, 'of', Effect, to, Owner, with, _Attack},
         _}) ->
    Log = [{?EVENT, Effect},
           {?SOURCE, Attacker},
           {?TARGET, Owner},
           ?RULES_MOD,
           {Effect, Amount}],
    case {HP, is_hp_effect(Effect)} of
        {Dead, true} when Dead =< 0 ->
            ?FAIL_NOSUB(no_damage_below_zero);
        {_, true} ->
            ?SUCCEED_SUB;
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
    undefined.

succeed({Props,
         {Attacker, cause, Amount, 'of', Effect, to, Owner, with, _Attack},
         Context}) ->
    Log = [{?EVENT, Effect},
           {?TARGET, Owner},
           ?RULES_MOD,
           {from, Attacker},
           {Effect, Amount}],
    {Props2, Log2} = maybe_take_damage(Attacker, Owner, Amount, Effect, Props, Context),
    {Props2, Log2 ++ Log};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

maybe_take_damage(Attack, Owner, Amount, Effect, Props, Context) ->
    CurrHp = proplists:get_value(hitpoints, Props),
    case CurrHp of
        AlreadyDead when AlreadyDead =< 0 ->
            {Props, []};
        _ ->
            take_damage(Attack, Owner, Amount, Effect, CurrHp, Props, Context)
    end.

take_damage(Attacker, Owner, Amount, EffectType, CurrHp, Props, Context) ->
    Owner = proplists:get_value(owner, Props),
    MaxHp = proplists:get_value(max, Props, 100_000_000),
    NewHp = min(CurrHp - Amount, MaxHp),
    Log = [{hp, NewHp}],
    IsHealing = proplists:get_value(is_healing, Props, false),


    case {NewHp, IsHealing} of
        {Death, _} when Death =< 0 ->
            egre:attempt(Owner,
                         {Attacker, killed, Owner, with, EffectType},
                         Context,
                         _ShouldSubscribe = true);
        {MaxHp, true} ->
            egre:attempt(self(),
                         {Owner, hitpoints, at, max},
                         #{},
                         false);
        {_, false} ->
            egre:attempt(self(),
                         {Owner, hitpoints, below, max},
                         #{},
                         false);
        {_, _} ->
            ok
    end,
    Props2 = lists:keystore(hitpoints, 1, Props, {hitpoints, NewHp}),
    {Props2, Log}.

is_hp_effect(blunt_force) ->
    true;
is_hp_effect(fire) ->
    true;
is_hp_effect(heal) ->
    true;
is_hp_effect(_) ->
    false.

