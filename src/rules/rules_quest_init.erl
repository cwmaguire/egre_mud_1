-module(rules_quest_init).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt(_) ->
    undefined.

succeed({Props, {_Self, init}}) ->
    Log = [{?EVENT, init_quest},
           {?SOURCE, self()},
           {?TARGET, self()}],
    IsComplete = proplists:get_value(is_complete, Props, false),
    IsTurnedIn = proplists:get_value(is_turned_in, Props, false),
    Props2 = lists:keystore(is_complete, 1, Props, {is_complete, IsComplete}),
    Props3 = lists:keystore(is_turned_in, 1, Props2, {is_turned_in, IsTurnedIn}),
    {Props3, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

