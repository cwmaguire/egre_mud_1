%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_shout).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Room}, Props, {Self, shouts, Phrase}, _})
  when Self == self() ->
    Log = [{?EVENT, shouts},
           {?SOURCE, Self},
           {?TARGET, Room}],
    Name = proplists:get_value(name, Props),
    NewEvent = {Name, shouts, Phrase, in, Room},
    #result{result = {resend, Self, NewEvent},
            subscribe = ignored,
            props = Props,
            log = Log};
attempt({#{owner := Room}, Props, {Player, shouts, _Phrase, in, Room}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, shouts},
           {?TARGET, Room}],
    ?SUCCEED_SUB;
attempt({#{owner := Room}, Props,
         {Player, shouts, _Phrase, to, Room, from, _Exit},
         _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, shouts},
           {?TARGET, Room}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Player, shouts, Phrase, in, Room}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, shouts},
           {?TARGET, Room}],
    Conn = proplists:get_value(conn, Props),
    egre:attempt(Conn,
                 {send, self(), <<Player/binary, " shouts: ", Phrase/binary>>}),
    {Props, Log};
succeed({Props, {Player, shouts, Phrase, to, Room, from, Exit}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, shouts},
           {?TARGET, Room}],
    Conn = proplists:get_value(conn, Props),
    egre:attempt(Conn,
                 {send, self(),
                  <<Player/binary, " shouts: ",
                    Phrase/binary, " from ",
                    (atom_to_binary(Exit))/binary>>}),
    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
