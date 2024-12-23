%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_time_effect).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{},
         Props,
         {Character, cause, EffectAmount, 'of', EffectType,
          to, Target,
          with, Effect},
         Context}) ->
    Log = [{?EVENT, add_effect_context},
           {?SOURCE, Character},
           {?TARGET, Target}],
    Context2 = [{time, proplists:get_value(time, Props, <<"missing time">>)} | Context],
    NewEvent = {Character, cause, EffectAmount, 'of', EffectType,
                to, Target,
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
