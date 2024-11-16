%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_spell_ack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Self, register, Effect, effect}, _})
  when Self == self() ->
    Log = [{?EVENT, effect_registration},
           {?SOURCE, Effect},
           {?TARGET, self()},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {_Self, register, Effect, effect}, _}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, effect_registration},
           {?TARGET, self()},
           ?RULES_MOD],
    egre:attempt(Effect, {self(), registered, Effect, effect}, false),
    {[{effect, Effect} | Props], Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
