%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_hitpoints_recover).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{},
         Props,
         {Self, init},
         _})
  when Self == self() ->
    Log = [{?EVENT, init},
           {?SOURCE, Self},
           {?TARGET, Self},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{},
         Props,
         {Resource, allocate, Required, 'of', healing, to, Self, to, heal},
         _})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Required},
           {resource_type, healing},
           {?SOURCE, Resource},
           {?TARGET, Self},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{owner := Owner},
         Props,
         {Attacker, killed, Owner, with, _EffectType}, _}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Attacker},
           {?TARGET, Owner},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{owner := Owner,
           is_healing := false},
         Props,
         {Owner, hitpoints, below, max},
         _}) ->
    Log = [{?EVENT, hp_not_max},
           {?SOURCE, Owner},
           {?TARGET, Owner},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{owner := Owner},
         Props,
         {Owner, hitpoints, at, max},
         _}) ->
    Log = [{?EVENT, hp_at_max},
           {?SOURCE, Owner},
           {?TARGET, Owner},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.


succeed({Props, {Self, init}, _}) ->
    Log = [{?EVENT, init},
           {?SOURCE, Self},
           {?TARGET, Self},
           ?RULES_MOD],
    {lists:keystore(is_healing, 1, Props, {is_healing, false}),
     Log};
succeed({Props, {Attacker, killed, Owner, with, _EffectType}, _}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Attacker},
           {?TARGET, Owner},
           ?RULES_MOD],
            egre:attempt(self(),
                         {Owner, unreserve, healing, for, self()},
                         #{},
                         false),
    Props2 = lists:keystore(is_healing, 1, Props, {is_healing, false}),
    {Props2, Log};
succeed({Props, {Owner, hitpoints, below, max}, _}) ->
    Log = [{?EVENT, hp_not_max},
           {?SOURCE, Owner},
           {?TARGET, Owner},
           ?RULES_MOD],
            egre:attempt(self(),
                         {Owner, reserve,
                          1, 'of', healing,
                          for, self(),
                          to, heal,
                          infinity, times},
                         #{},
                         false),
    Props2 = lists:keystore(is_healing, 1, Props, {is_healing, true}),
    {Props2, Log};
succeed({Props, {Owner, hitpoints, at, max}, _}) ->
    Log = [{?EVENT, hp_at_max},
           {?SOURCE, Owner},
           {?TARGET, Owner},
           ?RULES_MOD],
    egre:attempt(self(),
                 {Owner, unreserve, healing, for, self()},
                 #{},
                 false),
    Props2 = lists:keystore(is_healing, 1, Props, {is_healing, false}),
    {Props2, Log};

succeed({Props, {Resource, allocate, Amt, 'of', healing, to, Self, to, heal}, _})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Amt},
           {resource_type, healing},
           {?SOURCE, Resource},
           {?TARGET, Self},
           {rules_module, attack}],

    Character = proplists:get_value(owner, Props),
    Event = {Character, cause, -1, 'of', heal, to, Character, with, Self},
    egre_object:attempt(self(), Event, false),

    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
