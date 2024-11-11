%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_delete_property).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {delete, Pid}, _}) ->
    Log = [{?SOURCE, Pid},
           {?EVENT, delete}],
    #result{result = succeed,
            subscribe = false,
            props = lists:keydelete(Pid, 2, Props),
            log = Log};
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
