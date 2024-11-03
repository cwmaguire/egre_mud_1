%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_metrics).
-behaviour(egre_rules).
-compile({parse_transform,
          egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner}, Props, {Owner, metrics, get, _Metric}}) ->
    Log = [{?EVENT, metrics},
           {?SOURCE, Owner},
           {?TARGET, self()}],
    ct:pal("~p rules_char_metrics attempt get metric ~p; "
           "Owner = ~p", [self(), _Metric, Owner]),
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt({#{owner := Owner}, Props, {Owner, metrics, add, _Metric, _Count}}) ->
    Log = [{?EVENT, metrics_add},
           {?SOURCE, Owner},
           {?TARGET, self()}],
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Owner, metrics, get, Metric}}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, init},
           {?TARGET, self()}],
    #{Metric := Count} = proplists:get_value(metrics, Props),
    ct:pal("~p rules_char_metrics succeed get metric ~p", [self(), Metric]),
    egre:attempt(Owner, {Owner, metrics, Metric, Count}, false),
    {Props, Log};
succeed({Props, {Owner, metrics, add, Metric, Count}}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, metrics_add},
           {?TARGET, self()}],
    Metrics = #{Metric := OldCount} = proplists:get_value(metrics, Props),
    UpdatedMetrics = Metrics#{Metric := OldCount + Count},
    NewProps = [{metrics, UpdatedMetrics} | lists:delete(metrics, Props)],

    {NewProps, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
