%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_achievement_got_wood_1).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner}, Props, {Owner, achievement, Self, ack}, _})
  when Self == self() ->
    Log = [{?EVENT, ack_achievement},
           {?SOURCE, Owner},
           {?TARGET, self()}],
    ?SUCCEED_SUB;
attempt({#{owner := Owner}, Props, {Owner, metrics, trees_chopped, _Count}, _}) ->
    Log = [{?EVENT, metrics},
           {?SOURCE, Owner},
           {?TARGET, self()}],
    ct:pal("~p rules_achievements_got_wood_1 attempt get metric trees_chopped", [self()]),
    ?SUCCEED_SUB;
attempt({#{owner := Owner}, Props, {Owner, chopped, tree, Tree}, _}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Owner},
           {?TARGET, Tree}],
    ?SUCCEED_SUB;
attempt({#{owner := Owner}, Props, {Owner, achieved, got_wood_1}, _}) ->
    Log = [{?EVENT, achieved},
           {?SOURCE, Owner},
           {?TARGET, self()}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {_, init}, _Context}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, init},
           {?TARGET, self()}],
    Owner = proplists:get_value(owner, Props),
    egre:attempt(Owner, {Owner, achievement, self()}, false),
    {Props, Log};
succeed({Props, {Owner, achievement, _Self, ack}, _Context}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, achievement_ack},
           {?TARGET, self()}],
    case proplists:get_value(allow_previous, Props) of
        true ->
          %Owner = proplists:get_value(owner, Props),
          egre:attempt(Owner, {Owner, metrics, get, trees_chopped}, false);
        _ ->
          ok
    end,
    {Props, Log};
succeed({Props, {Owner, metrics, trees_chopped, NumTreesChopped}, _Context}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, init},
           {?TARGET, self()}],
    NewProps = case proplists:get_value(allow_previous, Props) of
        true ->
            UpdatedCount = proplists:get_value(count, Props) + NumTreesChopped,
            ct:pal("~p:~p: UpdatedCount~n\t~p~n", [?MODULE, ?FUNCTION_NAME, UpdatedCount]),
            maybe_trigger_achievement(Owner, Props, UpdatedCount);
        _ ->
            Props
    end,
    {NewProps, Log};
succeed({Props, {Owner, chopped, tree, Tree}, _Context}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, chopped},
           {?TARGET, Tree}],
    UpdatedCount = proplists:get_value(count, Props) + 1,
    NewProps = maybe_trigger_achievement(Owner, Props, UpdatedCount),
    {NewProps, Log};
succeed({Props, {Owner, achieved, got_wood_1}, _Context}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, achieved},
           {?TARGET, self()}],
    egre:attempt(Owner, {send, Owner, <<"You achieved 'Got Wood?'!">>}),
    {Props, Log};
succeed({Props, _, _}) ->
    Props.

fail({Props, _, _, _}) ->
    Props.

maybe_trigger_achievement(Owner, Props, Count) ->
    Target = proplists:get_value(target, Props),
    IsDone = proplists:get_value(done, Props),
    PropsMinusCount = proplists:delete(count, Props),
    case {IsDone, Count} of
        {false, Enough} when Enough >= Target ->
            egre:attempt(Owner, {Owner, achieved, got_wood_1}),
            PropsMinusDone = proplists:delete(done, PropsMinusCount),
            [{done, true}, {count, Count} | PropsMinusDone];
        _ ->
            [{count, Count} | PropsMinusCount]
    end.
