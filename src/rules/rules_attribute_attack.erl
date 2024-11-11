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

%% Attack
attempt({#{character := Character,
           top_item := TopItem = #top_item{item = Item}},
         Props,
         {Character, calc, Hit, on, Target, with, Item}, _}) ->
    Log = [{?EVENT, calc_hit},
           {?SOURCE, Character},
           {?TARGET, Target},
           {hit, Hit},
           {item, Item}],
    case is_interested(TopItem, Props) of
        true ->
            case proplists:get_value(attack_hit_modifier, Props) of
                undefined ->
                    ?SUCCEED_NOSUB;
                Amount ->
                    #result{new_event = {Character, calc, Hit + Amount, on, Target, with, Item},
                            subscribe = false,
                            props = Props,
                            log = [{new_hit, Hit + Amount} | Log]}
            end;
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{character := Character,
           top_item := TopItem = #top_item{item = Item}},
         Props,
         {Character, damage, Damage, to, Target, with, Item}, _}) ->
    Log = [{?EVENT, damage},
           {?SOURCE, Character},
           {?TARGET, Target},
           {damage, Damage},
           {item, Item}],
    case is_interested(TopItem, Props) of
        true ->
            case proplists:get_value(attack_damage_modifier, Props) of
                undefined ->
                    ?SUCCEED_NOSUB;
                Amount ->
                    #result{new_event = {Character, calc, Damage + Amount, on, Target, with, Item},
                            subscribe = false,
                            props = Props,
                            log = [{new_damage, Damage + Amount} | Log]}
            end;
        _ ->
            ?SUCCEED_NOSUB
    end;

%% Defend with item
attempt({#{character := Character,
           top_item := TopItem = #top_item{item = Item}},
         Props,
         {Attacker, calc, Hit, on, Character, with, Item}, _}) ->
    Log = [{?EVENT, calc_hit},
           {?SOURCE, Attacker},
           {?TARGET, Character},
           {hit, Hit},
           {item, Item}],
    case is_interested(TopItem, Props) of
        true ->
            case proplists:get_value(defence_hit_modifier, Props) of
                undefined ->
                    ?SUCCEED_NOSUB;
                Amount ->
                    #result{new_event = {Attacker, calc, Hit - Amount, on, Character, with, Item},
                            subscribe = false,
                            props = Props,
                            log = [{new_hit, Hit - Amount} | Log]}
            end;
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{character := Character,
           top_item := TopItem = #top_item{item = Item}},
         Props,
         {Target, damage, Damage, to, Character, with, Item}, _}) ->
    Log = [{?EVENT, damage},
           {?SOURCE, Target},
           {?TARGET, Character},
           {item, Item}],
    case is_interested(TopItem, Props) of
        true ->
            case proplists:get_value(defence_damage_modifier, Props) of
                undefined ->
                    ?SUCCEED_NOSUB;
                Amount ->
                    {succeed,
                     {Target, calc, Damage - Amount, on, Character, with, Item},
                     false,
                     Props,
                     [{new_damage, Damage - Amount} | Log]}
            end;
        _ ->
            ?SUCCEED_NOSUB
    end;

%% Defend without item
attempt({#{character := Character},
         Props,
         {Attacker, calc, Hit, on, Character, with, Item}, _}) ->
    Log = [{?EVENT, calc_hit},
           {?SOURCE, Attacker},
           {?TARGET, Character},
           {item, Item}],
    case is_interested(not_an_item, Props) of
        true ->
            case proplists:get_value(defence_hit_modifier, Props) of
                undefined ->
                    ?SUCCEED_NOSUB;
                Amount ->
                    {succeed,
                     {Attacker, calc, Hit - Amount, on, Character, with, Item},
                     false,
                     Props,
                     [{new_hit, Hit - Amount} | Log]}
            end;
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{character := Character},
         Props,
         {Attacker, damage, Damage, to, Character, with, Item}, _}) ->
    Log = [{?EVENT, damage},
           {?SOURCE, Attacker},
           {?TARGET, Character},
           {item, Item}],
    case is_interested(not_an_item, Props) of
        true ->
            case proplists:get_value(defence_damage_modifier, Props) of
                undefined ->
                    ?SUCCEED_NOSUB;
                Amount ->
                    {succeed,
                     {Attacker, calc, Damage - Amount, on, Character, with, Item},
                     false,
                     Props,
                     [{new_damage, Damage - Amount} | Log]}
            end;
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
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

succeed({Props, _, _}) ->
    Props.

fail({Props, _, _, _}) ->
    Props.
