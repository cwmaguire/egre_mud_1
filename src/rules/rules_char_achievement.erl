%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_achievement).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Self, achievement, Achievement}, _})
  when Self == self() ->
    Log = [{?EVENT, achievement_registration},
           {?SOURCE, Achievement},
           {?TARGET, self()}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {_Self, achievement, Achievement}, _}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, achievement_registration},
           {?TARGET, self()}],
    egre:attempt(Achievement, {self(), achievement, Achievement, ack}, false),
    {[{achievement, Achievement} | Props], Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
