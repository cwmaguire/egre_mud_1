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

%% Attacking: hit and damage
attempt({#{top_item := TopItem = #top_item{is_wielded = true, is_active = true},
           attack_hit_modifier := AttackHitModifier,
           is_active := true},
         Props,
         {Character, calc, Hit, on, Target, with, TopItem},
         _}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, calc_hit},
           {hit, Hit},
           {?TARGET, Target},
           {vector, TopItem}],
    NewEvent = {Character, calc, Hit + AttackHitModifier, on, Target, with, TopItem},
    #result{result = succeed,
            new_event = NewEvent,
            subscribe = false,
            props = Props,
            log = Log};
attempt({#{top_item := TopItem = #top_item{is_wielded = true, is_active = true},
           attack_damage_modifier := AttackDamageModifier,
           is_active := true},
         Props,
         {Character, damage, Damage, to, Target, with, TopItem},
         _}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, damage},
           {damage, Damage},
           {?TARGET, Target},
           {vector, TopItem}],
    NewEvent = {Character, calc, Damage + AttackDamageModifier, on, Target, with, TopItem},
    #result{result = succeed,
            new_event = NewEvent,
            subscribe = false,
            props = Props,
            log = Log};

%% Defending: hit and damage
%% I'm going to have to have top items broadcast wielded
%% and active when their states change.
attempt({#{character := Character,
           top_item := #top_item{is_wielded = true, is_active = true},
           is_active := true,
           defence_hit_modifier := DefenceHitModifier},
         Props,
         {Attacker, calc, Hit, on, Character, with, AttackVector},
         _}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_hit},
           {hit, Hit},
           {?TARGET, Character},
           {vector, AttackVector}],
    NewEvent = {Character, calc, Hit + DefenceHitModifier, on, Character, with, AttackVector},
    #result{result = succeed,
            new_event = NewEvent,
            subscribe = false,
            props = Props,
            log = Log};
attempt({#{character := Character,
           top_item := #top_item{is_wielded = true, is_active = true},
           is_active := true,
           defence_damage_modifier := DefenceDamageModifier},
         Props,
         {Character, damage, Damage, to, Target, with, AttackVector},
         _}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, damage},
           {damage, Damage},
           {?TARGET, Target},
           {vector, AttackVector}],
    NewEvent = {Character, calc, Damage + DefenceDamageModifier, on, Target, with, AttackVector},
    #result{result = succeed,
            new_event = NewEvent,
            subscribe = false,
            props = Props,
            log = Log};
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
