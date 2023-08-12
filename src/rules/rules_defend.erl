%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_handler_defend).
-behaviour(egre_handler).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include_lib("egre/include/egre.hrl").


%% Defend
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, HitRoll, on, Character, with, AttackType}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_hit},
           {hit, HitRoll},
           {?TARGET, Character},
           {attack_type, AttackType}],
    case should_defend(Props) of
        true ->
            %case egre_modifiers:modifier(Props, defence, hit, AttackType) of
            case proplists:get_value(defence_hit_roll, Props, {0, 0}) of
                {0, 0} ->
                    {succeed, false, Props, Log};
                {MaybeRoll, Base} ->
                    DefenceRoll = roll(MaybeRoll, Base),
                    {succeed,
                     {Attacker, calc, HitRoll - DefenceRoll, on, Character, with, AttackType},
                     true,
                     Props,
                     Log}
            end;
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, EffectRoll, on, Character, with, Effect}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_damage},
           {effect_roll, EffectRoll},
           {?TARGET, Character},
           {effect, Effect}],
    case should_defend(Props) of
        true ->
            case proplists:get_value(defence_effect_roll, Props, {0, 0}) of
                {_Roll = 0, _Base = 0} ->
                    {succeed, false, Props, Log};
                {MaybeRoll, Base} ->
                    DefenceRoll = roll(MaybeRoll, Base),
                    {succeed,
                     {Attacker, calc, EffectRoll - DefenceRoll, damage, Character},
                     true,
                     Props,
                     Log}
            end;
        _ ->
            {succeed, false, Props, Log}
    end;

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

should_defend(Props) ->
    ShouldDefendModule = proplists:get_value(should_defend_module, Props),
    ShouldDefendModule:should_defend(Props).

roll(_Roll = 0, Base) ->
    Base;
roll(Roll, Base) ->
    rand:uniform(Roll) + Base.

%% log(Props) ->
%%     egre_event_log:log(debug, [{module, ?MODULE} | Props]).
