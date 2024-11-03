%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_achievement).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Self, achievement, Achievement}})
  when Self == self() ->
    Log = [{?EVENT, achievement_registration},
           {?SOURCE, Achievement},
           {?TARGET, self()}],
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {_Self, achievement, Achievement}}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, achievement_registration},
           {?TARGET, self()}],
    egre:attempt(Achievement, {self(), achievement, Achievement, ack}, false),
    {[{achievement, Achievement} | Props], Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
