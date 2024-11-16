%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_hitpoints_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{owner := Owner},
         Props,
         {Attacker, cause, Amount, 'of', Effect, to, Owner, with, _Attack},
         _}) ->
    Log = [{?EVENT, Effect},
           {?SOURCE, Attacker},
           {?TARGET, Owner},
           {Effect, Amount}],
    case is_hp_effect(Effect) of
        true ->
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
           {from, Attacker},
           {Effect, Amount}],
    {Props2, Log2} = take_damage(Attacker, Owner, Amount, Effect, Props, Context),
    {Props2, Log2 ++ Log};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

take_damage(Attacker, Owner, Amount, EffectType, Props, Context) ->
    Owner = proplists:get_value(owner, Props),
    Hp = proplists:get_value(hitpoints, Props, 0) - Amount,
    ct:pal("~p:~p: Hp~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Hp]),
    Log = [{hp, Hp}],


    case Hp of
        X when X < 1 ->
            Owner = proplists:get_value(owner, Props),
            ct:pal("~p trying to call egre:attempt(~p, {~p, killed, ~p, with, ~p}, ~p, _ShouldSubscribe = true)",
                   [self(), Owner, Attacker, Owner, EffectType, Context]),
            egre:attempt(Owner,
                         {Attacker, killed, Owner, with, EffectType},
                         Context,
                         _ShouldSubscribe = true);
        _ ->
            ok
    end,
    Props2 = lists:keystore(hitpoints, 1, Props, {hitpoints, Hp}),
    {Props2, Log}.

is_hp_effect(blunt_force) ->
    true;
is_hp_effect(fire) ->
    true;
is_hp_effect(heal) ->
    true;
is_hp_effect(_) ->
    false.

