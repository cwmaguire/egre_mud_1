%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_attribute_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

%% @doc modify an attack with this attribute's modifiers
%%
%% This is to model things like strength, charisma, dexterity,
%% race, etc.
%%
%% This attribute can be for a character, a body part or an item.
%%

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

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

is_interested(#top_item{is_wielded = true,
                        is_active = true},
              _Props) ->
    true;
is_interested(#top_item{}, Props) ->
    proplists:get_value(must_be_wielded, Props, false);
is_interested(_, _) ->
    %% Everything that isn't bound to an item is always active for now
    %% e.g. character attribute
    true.

get_modifier(_IsAttacker = true, hit, Props) ->
    proplists:get_value(attack_hit_modifier, Props, 0);
get_modifier(_IsAttacker = true, effect, Props) ->
    -(proplists:get_value(attack_damage_modifier, Props, 0));
get_modifier(_IsAttacker = false, hit, Props) ->
    proplists:get_value(defence_hit_modifier, Props, 0);
get_modifier(_IsAttacker = false, effect, Props) ->
    -(proplists:get_value(defence_damage_modifier, Props, 0)).
