%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_effect).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{type := Type},
         Props,
         {Character, cause, EffectAmount, 'of', EffectType,
          to, Self,
          with, Effect,
          with, Context},
         _})
  when Self == self() ->
    Log = [{?EVENT, add_effect_context},
           {?SOURCE, Character},
           {?TARGET, self()}],
    Context2 = [{kill, Type} | Context],
    NewEvent = {Character, cause,
                  EffectAmount, 'of', EffectType,
                  to, Self,
                  with, Effect,
                  with, Context2},
    #result{result = succeed,
            event = NewEvent,
            subscribe = false,
            props = Props,
            log = Log};
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
