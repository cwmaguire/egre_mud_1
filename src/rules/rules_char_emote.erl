%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_emote).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Room, name := Name}, Props, {Self, emotes, Emote}, _})
  when Self == self() ->
    Log = [{?EVENT, emote},
           {?SOURCE, Self}],
    NewEvent = {Self, Name, emotes, Emote, in, Room},
    #result{result = {resend, Self, NewEvent},
            subscribe = true,
            props = Props,
            log = Log};
attempt({#{owner := Room}, Props, {Player, _PlayerName, emotes, _Emote, in, Room}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, says},
           {?TARGET, Room}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {_Self, Player, emotes, Emote, in, Room}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, emote},
           {?TARGET, Room}],
    Conn = proplists:get_value(conn, Props),
    egre:attempt(Conn, {send, self(), <<Player/binary, " ", Emote/binary>>}),
    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
