%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_subitem_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

%% This handler is specific to sub-items and controls whether this sub-item
%% process can participate in an attack or not. The character will
%% kick off a generic attack and then the generic attack handler attached
%% to this item process will further kick off a process-specific attack
%% for this process. This handler will listen to that specific attack
%% for it's process and determine if the properties of this sub-item
%% allow for the sub-item to attack. This prevents us from having logic in
%% the generic handler that is specific to items.
%% The generic handler can work out kicking off the hit roll and damage
%% since other handlers and other processes (e.g. attributes) will modify
%% the hit roll and damage with logic specific to this character, body part,
%% item, etc.
%%
%% This is very similar to the item attack handler except this handler
%% checks if this sub-item belongs to an attacking or defending parent
%% item.

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{character := Character},
         Props,
         {Attacker, roll, HitOrEffectAmount,
          for, HitOrEffect,
          with, EffectType,
          on, Defender,
          with,
          attack_source, AttackSource,
          effect, Effect},
         _}) ->
    Log = [{?EVENT, HitOrEffect},
           {?SOURCE, Attacker},
           {?TARGET, Defender},
           ?RULES_MOD,
           {roll, HitOrEffectAmount},
           {item, AttackSource}],
    TopItem = proplists:get_value(top_item, Props),
    case is_interested(TopItem, Props) of
        true ->
            CharIsAttacker = Character == Attacker,
            case get_modifier(CharIsAttacker, HitOrEffect, Props) of
                undefined ->
                    ?SUCCEED_NOSUB;
                Modifier ->
                    Event = {Attacker, roll, HitOrEffectAmount + Modifier,
                             for, HitOrEffect,
                             with, EffectType,
                             on, Defender,
                             with,
                             attack_source, AttackSource,
                             effect, Effect},
                    #result{event = Event,
                            subscribe = false,
                            props = Props,
                            log = [{new_roll, HitOrEffectAmount + Modifier} | Log]}
            end;
        _ ->
            ?SUCCEED_NOSUB
    end;

attempt(_) ->
    undefined.

succeed({Props, {Character, attack, Target}, _}) when is_pid(Target) ->
    Log = [{?SOURCE, Character},
           {?EVENT, attack},
           {?TARGET, Target}],
    egre_object:attempt(self(), {Character, attack, Target, with, self()}),
    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

is_interested(#top_item{is_wielded = true,
                        is_active = true},
              _Props) ->
    true;
is_interested(#top_item{is_active = true}, Props) ->
    not proplists:get_value(must_be_wielded, Props, false);
is_interested(_, _) ->
    false.

get_modifier(_IsAttacker = true, hit, Props) ->
    proplists:get_value(attack_hit_modifier, Props, 0);
get_modifier(_IsAttacker = true, effect, Props) ->
    -(proplists:get_value(attack_damage_modifier, Props, 0));
get_modifier(_IsAttacker = false, hit, Props) ->
    proplists:get_value(defence_hit_modifier, Props, 0);
get_modifier(_IsAttacker = false, effect, Props) ->
    -(proplists:get_value(defence_damage_modifier, Props, 0)).
