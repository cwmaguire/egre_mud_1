%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_attribute_modify).
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
           type := Type},
         Props,
         {add, _Amount, 'of', attribute, Type, to, Character}, _}) ->
    Log = [{?EVENT, modify_attribute},
           {?SOURCE, Character},
           {?TARGET, self()}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {add, X, 'of', attribute, _Type, to, Character}, _}) ->
    Log = [{?EVENT, modify_attribute},
           {?SOURCE, Character},
           {?TARGET, self()},
           ?RULES_MOD],
    Amount = proplists:get_value(amount, Props, 0),
    Props2 = lists:keystore(amount, 1, Props, {amount, Amount + X}),
    {Props2, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
