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
         {Attacker, calc, HitRoll, on, Character, with, AttackType},
         _}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_hit},
           {hit, HitRoll},
           {?TARGET, Character},
           {attack_type, AttackType}],
    case should_defend(Props) of
        true ->
            case proplists:get_value(defence_hit_roll, Props, {0, 0}) of
                {0, 0} ->
                    ?SUCCEED_NOSUB;
                {MaybeRoll, Base} ->
                    DefenceRoll = roll(MaybeRoll, Base),
                    NewEvent = {Attacker, calc, HitRoll - DefenceRoll, on, Character, with, AttackType},
                    ?SUCCEED_SUB_NEW_EVENT(NewEvent)
            end;
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{character := Character},
         Props,
         {Attacker, calc, EffectRoll, on, Character, with, Effect},
         _}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_damage},
           {effect_roll, EffectRoll},
           {?TARGET, Character},
           {effect, Effect}],
    case should_defend(Props) of
        true ->
            case proplists:get_value(defence_effect_roll, Props, {0, 0}) of
                {_Roll = 0, _Base = 0} ->
                    ?SUCCEED_NOSUB;
                {MaybeRoll, Base} ->
                    DefenceRoll = roll(MaybeRoll, Base),
                    NewEvent = {Attacker, calc, EffectRoll - DefenceRoll, damage, Character},
                    ?SUCCEED_SUB_NEW_EVENT(NewEvent)
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

%% log(Props) ->
%%     egre_event_log:log(debug, [{module, ?MODULE} | Props]).
