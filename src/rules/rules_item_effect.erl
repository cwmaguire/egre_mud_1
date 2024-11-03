%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_item_effect).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{character := Character,
           body_part := {body_part, _Pid, BodyPart, _Ref},
           wearing_body_parts := WearingBodyParts,
           type := Type,
           is_clothing := true},
         Props,
         {Character, cause, EffectAmount, 'of', EffectType,
          to, Target,
          with, Effect,
          with, Context}}) ->
    Log = [{?EVENT, add_effect_context},
           {?SOURCE, Character},
           {?TARGET, self()}],
    case lists:member(BodyPart, WearingBodyParts) of
        true ->
            Context2 = [{wearing, Type} | Context],
            NewMessage = {Character, cause,
                          EffectAmount, 'of', EffectType,
                          to, Target,
                          with, Effect,
                          with, Context2},
            {succeed, NewMessage, _ShouldSubscribe = false, Props, Log};
        _ ->
            {succeed, _ShouldSubscribe = false, Props, Log}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
