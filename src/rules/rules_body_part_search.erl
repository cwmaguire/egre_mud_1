%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_body_part_search).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#parents{owner = Owner},
         Props,
         {Searcher, search, Owner, named, Name, with, body_parts, BodyParts}}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Searcher},
           {?TARGET, Owner}],
    Self = self(),
    case lists:member(Self, BodyParts) of
        false ->
            NewMessage = {Searcher, search, Owner, named, Name, with, body_parts, [Self| BodyParts]},
            {succeed, NewMessage, false, Props, Log};
        true ->
            {succeed, false, Props, Log}
    end;
attempt(_) ->
    undefined.

succeed({Props, _Msg}) ->
    Props.

fail({Props, _Result, _Msg}) ->
    Log = [{?EVENT, search},
           {?TARGET, self()}],
    {Props, Log}.
