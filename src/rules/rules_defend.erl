%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_defend).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").


%% Defend
attempt({#{character := Character},
         Props,
         {Attacker, roll, HitOrEffectAmount,
          for, HitOrEffect,
          with, _EffectType,  %% TODO eventually use this to see if armor is effective
          on, Character,
          with,
          attack_source, AttackSource,
          effect, _Effect},
         _}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, HitOrEffect},
           {?TARGET, Character},
           ?RULES_MOD,
           {roll_amount, HitOrEffectAmount},
           {attack_source, AttackSource}],
    case should_defend(Props) of
        true ->
            case proplists:get_value(defence_hit_roll, Props, {0, 0}) of
                {0, 0} ->
                    ?SUCCEED_NOSUB;
                {MaybeRoll, Base} ->
                    DefenceRoll = roll(MaybeRoll, Base),

                    NewEvent = {Attacker, roll, HitOrEffectAmount - DefenceRoll,
                                for, HitOrEffect,
                                with, _EffectType,
                                on, Character,
                                with,
                                attack_source, AttackSource,
                                effect, _Effect},

                    #result{event = NewEvent,
                            subscribe = false,
                            props = Props,
                            log = [{new_roll, HitOrEffectAmount - DefenceRoll} | Log]}
            end;
        _ ->
            ?SUCCEED_NOSUB
    end;

attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

should_defend(Props) ->
    ShouldDefendModule = proplists:get_value(should_defend_module, Props),
    ShouldDefendModule:should_defend(Props).

roll(_Roll = 0, Base) ->
    Base;
roll(Roll, Base) ->
    rand:uniform(Roll) + Base.
