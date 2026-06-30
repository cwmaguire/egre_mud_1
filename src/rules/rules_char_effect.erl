%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_effect).
-behaviour(egre_rules).
-compile([{property_type_module, mud_util},
          {parse_transform, egre_protocol_parse_transform}]).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{type := Type},
         Props,
         {Character, cause, EffectAmount, 'of', EffectType,
          to, Self,
          with, Effect},
         Context})
  when Self == self() ->
    Log = [{?EVENT, add_effect_context},
           {?SOURCE, Character},
           {?TARGET, self()}],
    Context2 = [{kill, Type} | Context],
    %% XXX The event doesn't seem changed at all.
    %% We've just added context.
    NewEvent = {Character, cause,
                  EffectAmount, 'of', EffectType,
                  to, Self,
                  with, Effect},
    #result{result = succeed,
            event = NewEvent,
            subscribe = false,
            props = Props,
            log = Log,
            context = Context2};
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
