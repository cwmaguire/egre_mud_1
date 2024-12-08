%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_effect_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{owner := Attack},
         Props,
         {Self, affect, Target, with, Attack},
         _})
  when Self == self() ->
    Log = [{?SOURCE, Attack},
           {?EVENT, affect},
           {?TARGET, Target}],
    ShouldSubscribe = proplists:get_value(target, Props, undefined) == Target,
    ?SUCCEED_MAYBE_SUB(ShouldSubscribe);

attempt({#{},
         Props,
         {_Character, roll, _Roll,
          for, HitOrEffect,
          with, EffectType,
          on, Target,
          with,
          attack_source, _AttackSource,
          effect, Self},
         _})
  when Self == self(),
       HitOrEffect == hit orelse HitOrEffect == effect ->
    Log = [{?SOURCE, Self},
           {?EVENT, roll_for_effect},
           {?TARGET, Target},
           {effect_type, EffectType}],
    case proplists:get_value(type, Props) of
        EffectType ->
            ?SUCCEED_SUB;
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
    undefined.

succeed({Props, {_Self, affect, Target, with, AttackSource}, _}) ->
    AttackSource = proplists:get_value(owner, Props),
    Log = [{?SOURCE, AttackSource},
           {?EVENT, affect},
           {?TARGET, Target},
           {rules_module, ?MODULE},
           {attack_source, AttackSource}],
    Character = proplists:get_value(character, Props),
    EffectType = proplists:get_value(type, Props),
    Event = {Character, roll, calc_hit_roll(Props),
             for, hit,
             with, EffectType,
             on, Target,
             with,
             attack_source, AttackSource,
             effect, self()},
    egre_object:attempt(self(), Event),

    {Props, Log};

succeed({Props, {Character, roll, SuccessRoll,
                 for, hit,
                 with, EffectType,
                 on, Target,
                 with,
                 attack_source, AttackSource,
                 effect, Self}, _})
  when is_pid(Target),
       SuccessRoll > 0,
       self() == Self ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_hit},
           {?TARGET, Target},
           ?RULES_MOD,
           {amount, SuccessRoll},
           {attack_source, AttackSource},
           {effect_type, EffectType},
           {effect, Self}],

    Event = {Character, roll, calc_effect_roll(Props),
             for, effect,
             with, EffectType,
             on, Target,
             with,
             attack_source, AttackSource,
             effect, Self},
    egre_object:attempt(self(), Event),
    {Props, Log};

succeed({Props, {Character, roll, FailRoll,
                 for, hit,
                 with, _EffectType,
                 on, Target,
                 with,
                 attack_source, _AttackSource,
                 effect, Self}, _})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_hit},
           {?TARGET, Target},
           {rules_module, ?MODULE},
           {amount, FailRoll},
           {effect, Self}],

    Msg =
        <<"{attacker} misses {target} with ",
          (proplists:get_value(name, Props))/binary>>,
    Placeholders = [{Character, <<"{attacker}">>},
                    {Target, <<"{target}">>}],
    egre:attempt(Character, {send, {room, 'of', Target}, Msg, Placeholders}, _Sub0 = false),

    {Props, Log};

succeed({Props, {Character, roll, EffectAmount,
                 for, effect,
                 with, EffectType,
                 on, Target,
                 with,
                 attack_source, _AttackSource,
                 effect, Self}, _})
  when is_pid(Target),
       Self == self(),
       EffectAmount /= 0 ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {?TARGET, Target},
           ?RULES_MOD,
           {amount, EffectAmount},
           {effect, Self}],

    EffectEvent = {Character, cause, EffectAmount,
                   'of', EffectType,
                   to, Target,
                   with, Self},
    egre:attempt(Target, EffectEvent, true),

    {Props, Log};

succeed({Props, {Character, roll, IneffectiveAmount,
                 for, effect,
                 with, _EffectType,
                 on, Target,
                 with,
                 attack_source, _AttackSource,
                 effect, Self},
         _})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, IneffectiveAmount},
           {?TARGET, Target},
           ?RULES_MOD,
           {effect, Self}],

    Msg =
        <<"{attacker} has no effect on {target} with ",
          (proplists:get_value(name, Props))/binary>>,
    Placeholders = [{Character, <<"{attacker}">>},
                    {Target, <<"{target}">>}],
    egre:attempt(Character, {send, {room, 'of', Target}, Msg, Placeholders}, _Sub0 = false),

    {Props, Log};

succeed({Props, {Attacker, cause, EffectAmount, 'of', EffectType, to, Target, with, Self}, _})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Attacker},
           {?TARGET, Target},
           {?EVENT, affect},
           ?RULES_MOD,
           {effect_type, EffectType}],

    AmountBin = <<"[", (mud_util:itob(abs(EffectAmount)))/binary, "]">>,
    TargetName = proplists:get_value(name, Props, <<"[no name]">>),
    Effect =
        case EffectAmount of
            Positive when Positive > 0 ->
                <<"damage">>;
            _ ->
                <<"healing">>
        end,
    Msg =
        <<"{attacker} does ",
          AmountBin/binary,
          " ",
          TargetName/binary,
          " ",
          Effect/binary,
          " to {target}">>,
    Placeholders = [{Attacker, <<"{attacker}">>},
                    {Target, <<"{target}">>}],
    egre:attempt(Attacker, {send, {room, 'of', Target}, Msg, Placeholders}, _Sub0 = false),

    %{Props, Log};
    maybe_repeat(Props, Log);

succeed({Props, {stop, Self}, _}) when Self == self() ->
    {stop, finished, Props, _LogProps = []};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

calc_hit_roll(Props) ->
    Roll = proplists:get_value(hit_roll, Props, {0, 0}),
    mud_roll:roll(Roll).

calc_effect_roll(Props) ->
    EffectAmount = proplists:get_value(effect_roll, Props, 0),
    mud_roll:roll(EffectAmount).

% TODO implement repeat logic for effects that keep going
% maybe re-roll for hit?
% maybe check wait time?
maybe_repeat(Props, Log) ->
    stop(Props, Log).

stop(Props, Log) ->
    Owner = proplists:get_value(owner, Props),
    StopEvent = {delete, self()},
    egre_object:attempt(Owner, StopEvent),
    {stop, finished, Props, Log}.


%log(Props) ->
    %egre_event_log:log(debug, [{module, ?MODULE} | Props]).

