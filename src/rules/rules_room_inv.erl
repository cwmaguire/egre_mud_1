%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_room_inv).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{}, Props, {Item, move, from, Source, to, Target}, Context})
  when is_pid(Item) ->
    Log = [{?SOURCE, Item},
           {?EVENT, move},
           {from, Source},
           {to, Target}],
    Name = proplists:get_value(name, Props),
    ?SUCCEED_SUB([{room, self()}, {room_name, Name} | Context]);
attempt(_) ->
    undefined.

succeed({Props, {Item, move, from, Self, to, Target}, _}) when Self == self() ->
    log([<<"Process ">>, Target, <<" got ">>, Item, <<" from me">>]),
    Log = [{?SOURCE, Item},
           {?EVENT, move},
           {from, Self},
           {to, Target}],
    Props2 = lists:keydelete(Item, 2, Props),
    {Props2, Log};
succeed({Props, {Item, move, from, Target, to, Self}, _}) when Self == self() ->
    log([Item, <<" added to me from ">>, Target]),
    Log = [{?SOURCE, Item},
           {?EVENT, move},
           {from, Target},
           {to, Self}],
    Props2 = [{item, Item} | Props],
    {Props2, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

log(Terms) ->
    egre_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
