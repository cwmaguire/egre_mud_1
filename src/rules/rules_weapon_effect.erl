%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_weapon_effect).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{character := Character,
           body_part := {body_part, _Pid, BodyPart, _Ref},
           wielding_body_parts := WieldingBodyParts},
         Props,
         {Character, cause,
          EffectAmount, 'of', EffectType,
          to, Target,
          with, Effect,
          with, Context},
         _}) ->
    Log = [{?EVENT, add_effect_context},
           {?SOURCE, Character},
           {?TARGET, self()}],
    case lists:member(BodyPart, WieldingBodyParts) of
        true ->
            Context2 = [{wielding, proplists:get_value(type, Props, <<"missing weapon type">>)} | Context],
            NewEvent = {Character, cause,
                          EffectAmount, 'of', EffectType,
                          to, Target,
                          with, Effect,
                          with, Context2}, %% TODO move context to record field
            #result{result = succeed,
                    new_event = NewEvent,
                    subscribe = false,
                    props = Props,
                    log = Log};
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
