%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_item_inject_self).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{},
         Props,
         {Object, Action, ItemName},
         _})
  when is_binary(ItemName) andalso
       (Action == get orelse
        Action == drop orelse
        Action == look) ->
    Log = [{?EVENT, inject_self},
           {action, Action},
           {name, ItemName}],
    case is_name(Props, ItemName) of
        true ->
            NewEvent = {Object, Action, self()},
            #result{result = {resend, Object, NewEvent},
                    subscribe = false,
                    props = Props,
                    log = Log};
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{}, Props, {ItemName, move, from, Source, to, Target}, _})
  when is_binary(ItemName) ->
    Log = [{?EVENT, inject_self},
           {sub_type, move},
           {name, ItemName}],
    case is_name(Props, ItemName) of
        true ->
            NewEvent = {self(), move, from, Source, to, Target},
            #result{result = {resend, self(), NewEvent},
                    subscribe = false,
                    props = Props,
                    log = Log};
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

is_name(Props, Name) ->
    ItemName = proplists:get_value(name, Props, ""),
    match == re:run(ItemName, Name, [{capture, none}]).
