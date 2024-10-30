%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_shout).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Room}, Props, {Self, shouts, Phrase}})
  when Self == self() ->
    Log = [{?EVENT, shouts},
           {?SOURCE, Self}],
    Name = proplists:get_value(name, Props),
    NewMessage = {Name, shouts, Phrase, in, Room},
    {{resend, Self, NewMessage}, _ShouldSubscribe = ignored, Props, Log};
attempt({#parents{owner = Room}, Props, {Player, shouts, _Phrase, in, Room}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, shouts},
           {?TARGET, Room}],
    {succeed, _Subscribe = true, Props, Log};
attempt({#parents{owner = Room}, Props,
         {Player, shouts, _Phrase, to, Room, from, _Exit}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, shouts},
           {?TARGET, Room}],
    {succeed, _Subscribe = true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Player, shouts, Phrase, in, Room}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, shouts},
           {?TARGET, Room}],
    Conn = proplists:get_value(conn, Props),
    egre:attempt(Conn,
                 {send, self(), <<Player/binary, " shouts: ", Phrase/binary>>}),
    {Props, Log};
succeed({Props, {Player, shouts, Phrase, to, Room, from, Exit}}) ->
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
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
