%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_body_part_cleanup).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{owner := Character},
         Props,
         {Character, cleanup, body_parts, BodyParts, in, Room}, _}) ->
    Self = self(),
    Log = [{?SOURCE, Character},
           {?TARGET, Self},
           {room, Room},
           {?EVENT, cleanup_bodyparts}],
    case lists:member(Self, BodyParts) of
        false ->
            NewEvent = {Character, cleanup, body_parts, [Self | BodyParts], in, Room},
            #result{result = succeed,
                    event = NewEvent,
                    subscribe = false,
                    props = Props,
                    log = Log};
        true ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

