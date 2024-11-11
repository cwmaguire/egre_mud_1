%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_cleanup).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{owner := Room}, Props, {Self, cleanup}, _}) when Self == self() ->
    Log = [{?TARGET, Self},
           {?EVENT, cleanup}],
    Result = {resend, Self, {Self, cleanup, body_parts, [], in, Room}},
    #result{result = Result,
            subscribe = true,
            props = Props,
            log = Log};
attempt({#{}, Props, {Self, cleanup, body_parts, _BodyParts, in, _Room}, _})
  when Self == self() ->
    Log = [{?SOURCE, Self},
           {?TARGET, Self},
           {?EVENT, cleanup}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Self, cleanup, body_parts, _BodyParts, in, Room}, _}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?TARGET, Room},
           {?EVENT, cleanup}],
    egre_object:attempt(self(), {stop, self()}),
    {Props, Log};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
