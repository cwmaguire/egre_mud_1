%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_attack_resource).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

%% respond to resources being added to the owner by reserving
%% those resources and kicking off attacks
%%
%% 1) subscribe to resource increase
%% 2) on resource increase success kick off resource reservation
%% 3) on resource reservation success allocate resources
%% 4) if any attack has all the necessary resources then kick off attack
%% 5) on stop_attack unreserve resources

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{character := Character,
           owner := Owner},
         Props,
         {Character, attack, Target, with, Owner},
         _}) ->
    Log = [{?EVENT, attack},
           {?TARGET, Target}],
    ?SUCCEED_SUB;

attempt({#{character := Character},
         Props,
         {move, Character, From, To, Exit},
         _}) ->
    Log = [{?EVENT, move},
           {from, From},
           {to, To},
           {exit, Exit}],
    ?SUCCEED_SUB;

attempt({#{character := Character},
         Props,
         {Character, die},
         _}) ->
    Log = [{?EVENT, die}],
    ?SUCCEED_SUB;

attempt(_) ->
    undefined.

succeed({Props, {Character, attack, Target, with, Owner}, _}) ->
    Resources = proplists:get_value(resources, Props, []),
    [reserve(Character, Resource, Amount, Owner) || {Resource, Amount} <- Resources],
    Log = [{?EVENT, attack},
           {?TARGET, Target}],
    {Props, Log};

succeed({Props, {Character, move, From, To, Exit}, _}) ->
    Owner = proplists:get_value(owner, Props),
    [unreserve(Character, Resource, Owner) || {resource, Resource} <- Props],
    Log = [{?EVENT, move},
           {from, From},
           {to, To},
           {exit, Exit}],
    {Props, Log};

succeed({Props, {Character, die}, _}) ->
    Owner = proplists:get_value(owner, Props),
    unreserve(Character, Owner, Props),
    Log = [{?EVENT, die}],
    {Props, Log};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

reserve(Character, Resource, Amount, Owner) ->
    egre_object:attempt(self(), {Character, reserve, Amount, 'of', Resource, for, Owner}).

unreserve(Character, Owner, Props) when is_list(Props) ->
    [unreserve(Character, Resource, Owner) || {resource, Resource} <- Props];

unreserve(Character, Resource, Owner) ->
    egre_object:attempt(self(), {Character, unreserve, Resource, for, Owner}).
