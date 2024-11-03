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
          with, Context}})
  when Self == self() ->
    Log = [{?EVENT, add_effect_context},
           {?SOURCE, Character},
           {?TARGET, self()}],
    Context2 = [{kill, Type} | Context],
    NewMessage = {Character, cause,
                  EffectAmount, 'of', EffectType,
                  to, Self,
                  with, Effect,
                  with, Context2},
    {succeed, NewMessage, _ShouldSubscribe = false, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
