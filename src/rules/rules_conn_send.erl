%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_conn_send).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner}, Props, {send, Owner, Message}, _}) ->
    Log = [{?EVENT, send},
           {?TARGET, Owner},
           {player_message, Message}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.


succeed({Props, {send, Player, Message}, _}) ->
    Log = [{?EVENT, send},
           {?TARGET, Player},
           {player_message, Message},
           ?RULES_MOD],
    {Conn} = proplists:get_value(conn, Props),
    egremud_conn:handle(Conn, {send, Message}),
    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.
