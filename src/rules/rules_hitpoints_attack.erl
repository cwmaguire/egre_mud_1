%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_hitpoints_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include_lib("egre/include/egre.hrl").

attempt({#parents{owner = Owner},
         Props,
         {Attacker, cause, Amount, 'of', Effect, to, Owner, with, _Attack}}) ->
    Log = [{?EVENT, Effect},
           {?SOURCE, Attacker},
           {?TARGET, Owner},
           {Effect, Amount}],
    case is_hp_effect(Effect) of
        true ->
            {succeed, true, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt(_) ->
    undefined.

succeed({Props,
         {Attacker, cause, Amount, 'of', Effect, to, Owner, with, _Attack}}) ->
    Log = [{?EVENT, Effect},
           {?TARGET, Owner},
           {from, Attacker},
           {Effect, Amount}],
    {Props2, Log2} = take_damage(Attacker, Owner, Amount, Effect, Props),
    {Props2, Log2 ++ Log};

succeed({Props, _Msg}) ->
    Props.

fail({Props, _Reason, _Message}) ->
    Props.

take_damage(Attacker, Owner, Amount, EffectType, Props) ->
    Owner = proplists:get_value(owner, Props),
    Hp = proplists:get_value(hitpoints, Props, 0) - Amount,
    Log = [{hp, Hp}],


    case Hp of
        X when X < 1 ->
            Owner = proplists:get_value(owner, Props),
            ct:pal("Sending 'killed' message for character ~p~n", [Owner]),
            egre_object:attempt(Owner, {Attacker, killed, Owner, with, EffectType});
        _ ->
            ok
    end,
    Props2 = lists:keystore(hitpoints, 1, Props, {hitpoints, Hp}),
    {Props2, Log}.

is_hp_effect(blunt_force) ->
    true;
is_hp_effect(fire) ->
    true;
is_hp_effect(_) ->
    false.

