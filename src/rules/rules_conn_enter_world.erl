%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_conn_enter_world).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {Owner, enter_world, in, Room, with, Self},
         _}) when Self == self(), is_pid(Room) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, enter_world},
           {room, Room},
           {conn, Self}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Player, enter_world, in, Room, with, Conn}, _}) ->
    Log = [{?EVENT, enter_world},
           {?SOURCE, self()},
           {conn, Conn},
           {room, Room},
           ?RULES_MOD],
    {[{owner, Player} | lists:keydelete(owner, 1, Props)], Log};
succeed(_) ->
    undefined.

%% TODO test that failing to enter the world disconnects the player
fail({Props, Reason, {Player, enter_world}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, enter_world}],
    Conn = proplists:get_value(conn, Props),
    Conn ! {disconnect, Reason},
    {Props, Log};
fail(_) ->
    undefined.
