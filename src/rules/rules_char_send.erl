%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_send).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{room := Room}, Props, {send, {room, Room}, _Message}, _}) ->
    Log = [{?EVENT, send},
           {?TARGET, Room},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {send, {room, Room}, Msg}, _}) ->
    Log = [{?SOURCE, Room},
           {?EVENT, send},
           {?TARGET, self},
           ?RULES_MOD],
    Event = {send, self(), Msg},
    egre:attempt(self(), Event, false),
    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
