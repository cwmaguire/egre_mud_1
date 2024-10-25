%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_say).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include_lib("egre/include/egre.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Room}, Props, {Self, says, Phrase}})
  when Self == self() ->
    Log = [{?EVENT, says},
           {?SOURCE, Self}],
    Name = proplists:get_value(name, Props),
    NewMessage = {Name, says, Phrase, in, Room},
    {{resend, Self, NewMessage}, _ShouldSubscribe = ignored, Props, Log};
attempt({#parents{owner = Room}, Props, {Player, says, Phrase, in, Room}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, says},
           {?TARGET, Room}],
    Conn = proplists:get_value(conn, Props),
    egre:attempt(Conn, {send, self(), <<Player/binary, " says: ", Phrase/binary>>}),
    {succeed, _Subscribe = false, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
