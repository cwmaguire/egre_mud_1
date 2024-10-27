%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_body_part_cleanup).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#parents{owner = Character},
         Props,
         {Character, cleanup, body_parts, BodyParts, in, Room}}) ->
    Self = self(),
    Log = [{?SOURCE, Character},
           {?TARGET, Self},
           {room, Room},
           {?EVENT, cleanup_bodyparts}],
    case lists:member(Self, BodyParts) of
        false ->
            NewMessage = {Character, cleanup, body_parts, [Self | BodyParts], in, Room},
            {succeed, NewMessage, false, Props, Log};
        true ->
            {succeed, false, Props, Log}
    end;
attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

