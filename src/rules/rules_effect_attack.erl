%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_effect_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include_lib("egre/include/egre.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATTEMPT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attempt({#parents{owner = Attack},
         Props,
         {Self, affect, Target}})
  when Self == self() ->
    Log = [{?SOURCE, Attack},
           {?EVENT, affect},
           {?TARGET, Target}],
    ShouldSubscribe = proplists:get_value(target, Props, undefined) == Target,
    {succeed, ShouldSubscribe, Props, Log};

attempt({_Parents,
         Props,
         {_Character, roll, _Roll, for, HitOrEffect, with, EffectType, on, Target, with, Self}})
  when Self == self(),
       HitOrEffect == hit; HitOrEffect == effect ->
    Log = [{?SOURCE, Self},
           {?EVENT, roll_for_effect},
           {?TARGET, Target},
           {effect_type, EffectType}],
    case proplists:get_value(type, Props) of
        EffectType ->
            {succeed, true, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;

attempt({_, _, _Msg}) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUCCEED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

succeed({Props, {_Self, affect, Target}}) ->
    Attack = proplists:get_value(owner, Props),
    Log = [{?SOURCE, Attack},
           {?EVENT, affect},
           {?TARGET, Target},
           {vector, Attack}],
    Character = proplists:get_value(character, Props),
    EffectType = proplists:get_value(type, Props),
    Event = {Character, roll, calc_hit_roll(Props), for, hit, with, EffectType, on, Target, with, self()},
    egre_object:attempt(self(), Event),

    {Props, Log};

succeed({Props, {Character, roll, SuccessRoll, for, hit, with, EffectType, on, Target, with, Attack}})
  when is_pid(Target),
       SuccessRoll > 0 ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_hit},
           {amount, SuccessRoll},
           {?TARGET, Target},
           {handler, ?MODULE},
           {vector, Attack},
           {effect_type, EffectType},
           {effect, self()}],

    Event = {Character, roll, calc_effect_roll(Props), for, effect, with, EffectType, on, Target, with, self()},
    egre_object:attempt(self(), Event),
    {Props, Log};

succeed({Props, {Character, roll, FailRoll, for, hit, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_hit},
           {amount, FailRoll},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    CharacterSubstitutions = [{<<"<target>">>, Target}],
    AmountBin = <<" [", (mud_util:itob(FailRoll))/binary, "]">>,
    CharacterMsg =
        <<"You miss <target> with ",
          (mud_util:atob(EffectType))/binary,
          AmountBin/binary>>,
    egre_object:attempt(Target, {send, Character, CharacterMsg, CharacterSubstitutions}),
    ct:pal("~p: CharacterMsg~n\t~p~n", [?MODULE, CharacterMsg]),

    TargetSubstitutions = [{<<"<character>">>, Character}],
    TargetMsg = <<"<character> misses you with ", (mud_util:atob(EffectType))/binary>>,
    TargetSubstitutions = [{<<"<target>">>, Target},
                           {<<"<character>">>, Character}],
    egre_object:attempt(Target, {send, Target, TargetMsg, TargetSubstitutions}),
    ct:pal("~p: TargetMsg~n\t~p~n", [?MODULE, TargetMsg]),
    {Props, Log};

succeed({Props, {Character, roll, EffectAmount, for, effect, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       EffectAmount > 0 ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, EffectAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    EffectEvent = {Character, cause, EffectAmount, 'of', EffectType, to, Target, with, Self},
    egre_object:attempt(Target, EffectEvent, false),

    maybe_repeat(Props, Log);

succeed({Props, {Character, roll, IneffectiveAmount, for, effect, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, IneffectiveAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    CharacterSubstitutions = [{<<"<target>">>, Target}],
    AmountBin = <<" [", (mud_util:itob(IneffectiveAmount))/binary, "]">>,
    CharacterMsg =
        <<(mud_util:atob(EffectType))/binary,
          " has no effect on <target> (",
          AmountBin/binary,
          ")">>,
    egre_object:attempt(Target, {send, Character, CharacterMsg, CharacterSubstitutions}),

    TargetSubstitutions = [{<<"<character>">>, Character}],
    TargetMsg = <<"<character>'s ", (mud_util:atob(EffectType))/binary, " has no effect">>,
    TargetSubstitutions = [{<<"<target>">>, Target},
                           {<<"<character>">>, Character}],
    egre_object:attempt(Target, {send, Target, TargetMsg, TargetSubstitutions}),
    {Props, Log};

succeed({Props, {Attacker, do, EffectAmount, 'of', EffectType, to, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Self},
           {?TARGET, Target},
           {?EVENT, affect},
           {handler, ?MODULE},
           {effect_type, EffectType}],

    AttackerSubstitutions = [{<<"<target>">>, self()}],
    AmountBin = <<" [", (mud_util:itob(EffectAmount))/binary, "]">>,
    AttackerMsg =
        <<"You do ",
          AmountBin/binary,
          " damage to <target> with ",
          (mud_util:atob(EffectType))/binary>>,
    egre_object:attempt(Attacker, {send, Attacker, AttackerMsg, AttackerSubstitutions}, _Sub0 = false),

    TargetSubstitutions = [{<<"<attacker>">>, Attacker}],
    TargetMsg = <<"<attacker> does ",
                  AmountBin/binary,
                  " damage to you with ",
                  (mud_util:atob(EffectType))/binary>>,
    egre_object:attempt(Target, {send, Target, TargetMsg, TargetSubstitutions}, _Sub1 = false),


    {Props, Log};

succeed({Props, {stop, Self}}) when Self == self() ->
    {stop, finished, Props, _LogProps = []};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

%% TODO move to target processes: e.g. HP, stamina, etc.
% affect(Props, EffectAmount) ->
%     Character = proplists:get_value(character, Props),
%     EffectType = proplists:get_value(effect_type, Props),
%     Target = proplists:get_value(target, Props),
%     NewMessage = {Character, Roll, for, EffectType, on, Target, with, self()},
%     egre_object:attempt(self(), NewMessage),
%
%    Target = proplists:get_value(target, Props),
%    AttackType = proplists:get_value(attack_type, Props, []),
%    Hit = calc_hit(Props),
%
%    Event = {Character, roll, 0, for, affect, with, EffectType, on, Target, with, Self},
%    % I think we're auto-subscribed
%    egre_object:attempt(self(), Event).

calc_hit_roll(Props) ->
    Roll = proplists:get_value(hit_roll, Props, {0, 0}),
    egre_roll:roll(Roll).

calc_effect_roll(Props) ->
    EffectAmount = proplists:get_value(effect_roll, Props, 0),
    egre_roll:roll(EffectAmount).

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

