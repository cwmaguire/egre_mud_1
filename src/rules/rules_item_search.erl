%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_item_search).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{owner := Char},
         Props,
         {Player, search, Char, named, _Name, with, body_parts, _BodyParts},
         _}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Char}],
    ?SUCCEED_SUB;
attempt({#{owner := Owner},
         Props,
         {Player, search, Char, named, _Name, with, body_parts, BodyParts},
         _}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Char}],
    ShouldSubscribe = lists:member(Owner, BodyParts),
    ?SUCCEED_MAYBE_SUB(ShouldSubscribe);
attempt(_) ->
    undefined.

succeed({Props,
         {Player, search, _Char, named, Name, with, body_parts, _BodyParts},
         _}) ->
    Context = <<Name/binary, " has ">>,
    send_description(Player, Props, Context),
    Props;
succeed(_) ->
    undefined.

fail({Props, _Result, _Msg, _}) ->
    Log = [{?EVENT, search},
           {?TARGET, self()}],
    {Props, Log}.

send_description(Source, Props, Context) ->
    Description = mud_util:describe(item, Props),
    SendMsg = {send, Source, [<<Context/binary>>, Description]},
    egre:attempt(Source, SendMsg).
