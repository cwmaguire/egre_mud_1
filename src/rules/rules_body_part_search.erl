%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_body_part_search).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{owner := Owner},
         Props,
         {Searcher, search, Owner, named, Name, with, body_parts, BodyParts},
         _}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Searcher},
           {?TARGET, Owner}],
    Self = self(),
    case lists:member(Self, BodyParts) of
        false ->
            NewEvent = {Searcher, search, Owner, named, Name, with, body_parts, [Self| BodyParts]},
            #result{result = succeed,
                    new_event = NewEvent,
                    subscribe = false,
                    props = Props,
                    log = Log};
        true ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
