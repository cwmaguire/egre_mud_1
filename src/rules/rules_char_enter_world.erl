%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_enter_world).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{},
         Props,
         {Self, enter_world, in, room, with, Conn}, _}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, enter_world},
           {conn, Conn}],
    case proplists:get_value(owner, Props) of
        undefined ->
            #result{result = succeed,
                    subscribe = false,
                    props = Props,
                    log = [{?TARGET, undefined} | Log]};
        Room ->
            NewEvent = {Self, enter_world, in, Room, with, Conn},
            #result{result = {resend, Self, NewEvent},
                    subscribe = true,
                    props = Props,
                    log = [{?TARGET, Room} | Log]}
    end;
attempt({#{},
         Props,
         {Self, enter_world, in, Room, with, _Conn}, _}) when Self == self(), is_pid(Room) ->
    Log = [{?SOURCE, Self},
           {?EVENT, enter_world},
           {?TARGET, Room}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Player, enter_world, in, Room, with, Conn}, _}) ->
    Log = [{?EVENT, char_enter_world},
           {?SOURCE, Player},
           {?TARGET, Room},
           ?RULES_MOD,
           {conn, Conn}],
    Props2 = lists:keystore(conn, 1, Props, {conn, Conn}),
    {Props2, Log};
succeed(_) ->
    undefined.

fail({Props, Reason, {Player, enter_world}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, enter_world}],
    Conn = proplists:get_value(conn, Props),
    Conn ! {disconnect, Reason},
    {Props, Log};
fail(_) ->
    undefined.
