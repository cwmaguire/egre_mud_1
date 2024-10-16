%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_conn_send).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include_lib("egre/include/egre.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner}, Props, {send, Owner, Message}}) ->
    Log = [{?EVENT, send},
           {?TARGET, Owner},
           {player_message, Message}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.


succeed({Props, {send, Player, Message}}) ->
    Log = [{?EVENT, send},
           {?TARGET, Player},
           {player_message, Message}],
    {Conn} = proplists:get_value(conn, Props),
    egre_conn:handle(Conn, {send, Message}),
    {Props, Log};
succeed({Props, _Other}) ->
    Props.

fail({Props, _Reason, _Message}) ->
    Props.
