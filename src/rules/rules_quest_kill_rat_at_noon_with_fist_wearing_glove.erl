%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_quest_kill_rat_at_noon_with_fist_wearing_glove).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {Owner, killed, Target, with, _, with, _Context}}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Owner},
           {?TARGET, Target}],
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Owner, killed, Target, with, _, with, Context}}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, killed},
           {?TARGET, Target}],
    case does_action_meet_criteria(Props, Context) of
       true ->
           Props2 = update_count(Props),
           {maybe_complete_quest(Props2), Log};
       _ ->
           {Props, Log}
    end;
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

does_action_meet_criteria(Props, Context) ->
    Objectives = sets:from_list(proplists:get_value(objectives, Props)),
    ContextSet = sets:from_list(Context),
    sets:is_subset(Objectives, ContextSet).

update_count(Props) ->
    CurrCount = proplists:get_value(count, Props),
    lists:keystore(count, 1, Props, {count, CurrCount + 1}).

maybe_complete_quest(Props) ->
    case has_enough_events(Props) of
        true ->
            lists:keystore(is_complete, 1, Props, {is_complete, true});
        _ ->
            Props
     end.

has_enough_events(Props) ->
    CurrCount = proplists:get_value(count, Props),
    TargetCount = proplists:get_value(target, Props),
    CurrCount >= TargetCount.
