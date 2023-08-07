%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(egre_handler_room_inv).
-behaviour(egre_handler).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/egre.hrl").

attempt({_Owner, Props, {Item, move, from, Source, to, Target}})
  when Source == self(); Target == self() ->
    Log = [{?SOURCE, Item},
           {?EVENT, move},
           {from, Source},
           {to, Target}],
    {succeed, true, Props, Log};
attempt(_Attempt) ->
    undefined.

succeed({Props, {Item, move, from, Self, to, Target}}) when Self == self() ->
    log([<<"Process ">>, Target, <<" got ">>, Item, <<" from me">>]),
    Log = [{?SOURCE, Item},
           {?EVENT, move},
           {from, Self},
           {to, Target}],
    Props2 = lists:keydelete(Item, 2, Props),
    {Props2, Log};
succeed({Props, {Item, move, from, Target, to, Self}}) when Self == self() ->
    log([Item, <<" added to me from ">>, Target]),
    Log = [{?SOURCE, Item},
           {?EVENT, move},
           {from, Target},
           {to, Self}],
    Props2 = [{item, Item} | Props],
    {Props2, Log};
succeed({Props, _}) ->
    {Props, _LogProps = []}.

fail({Props, _, _}) ->
    Props.

log(Terms) ->
    egre_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
