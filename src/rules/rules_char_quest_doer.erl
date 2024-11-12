%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_quest_doer).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Self, quest, Quest}, _})
  when Self == self() ->
    Log = [{?EVENT, quest_registration},
           {?SOURCE, Quest},
           {?TARGET, self()},
           {rules_module, char_quest_doer}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {_Self, quest, Quest}, _}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, quest_registration},
           {?TARGET, self()},
           ?RULES_MOD],
    egre:attempt(Quest, {self(), quest, Quest, ack}, false),
    {[{quest, Quest} | Props], Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
