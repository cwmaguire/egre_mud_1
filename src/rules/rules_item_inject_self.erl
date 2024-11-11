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
         {Object, Action, ItemName}})
  when is_binary(ItemName) andalso
       (Action == get orelse
        Action == drop orelse
        Action == look) ->
    case is_name(Props, ItemName) of
        true ->
            Log = [{?EVENT, inject_self},
                   {action, Action},
                   {name, ItemName}],
            NewEvent = {Object, Action, self()},
            #result{result = {resend, Object, NewEvent},
                    subscribe = false,
                    props = Props,
                    log = Log};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt({#{}, Props, {ItemName, move, from, Source, to, Target}})
  when is_binary(ItemName) ->
    case is_name(Props, ItemName) of
        true ->
            Log = [{?EVENT, inject_self},
                   {sub_type, move},
                   {name, ItemName}],
            NewEvent = {self(), move, from, Source, to, Target},
            #result{result = {resend, self(), NewEvent},
                    subscribe = false,
                    props = Props,
                    log = Log};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_name(Props, Name) ->
    ItemName = proplists:get_value(name, Props, ""),
    match == re:run(ItemName, Name, [{capture, none}]).

%log(Props) ->
    %egre_event_log:log(debug, [{module, ?MODULE} | Props]).
