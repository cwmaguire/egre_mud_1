%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_handler_char_cleanup).
-behaviour(egre_handler).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include_lib("egre/include/egre.hrl").

attempt({#parents{owner = Room}, Props, {Self, cleanup}}) when Self == self() ->
    Log = [{?TARGET, Self},
           {?EVENT, cleanup}],
    {{resend, Self, {Self, cleanup, body_parts, [], in, Room}}, true, Props, Log};
attempt({#parents{}, Props, {Self, cleanup, body_parts, _BodyParts, in, _Room}})
  when Self == self() ->
    {succeed, true, Props};
attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Self, cleanup, body_parts, _BodyParts, in, Room}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?TARGET, Room},
           {?EVENT, cleanup}],
    egre_object:attempt(self(), {stop, self()}),
    {Props, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
