%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_say).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Room, name := Name}, Props, {Self, says, Phrase}, _})
  when Self == self() ->
    Log = [{?EVENT, says},
           {?SOURCE, Self}],
    NewEvent = {Self, Name, says, Phrase, in, Room},
    #result{result = {resend, Self, NewEvent},
            subscribe = true,
            props = Props,
            log = Log};
attempt({#{owner := Room}, Props, {Player, _PlayerName, says, _Phrase, in, Room}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, says},
           {?TARGET, Room}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {_Self, Player, says, Phrase, in, Room}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, says},
           {?TARGET, Room}],
    Conn = proplists:get_value(conn, Props),
    egre:attempt(Conn, {send, self(), <<Player/binary, " says: ", Phrase/binary>>}),
    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
