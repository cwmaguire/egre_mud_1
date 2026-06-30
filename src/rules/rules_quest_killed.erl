%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_quest_killed).
-behaviour(egre_rules).
-compile([{property_type_module, mud_util},
          {parse_transform, egre_protocol_parse_transform}]).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner},
         Props,
         {Owner, killed, Target, with, _},
         _Context}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Owner},
           {?TARGET, Target}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Owner, killed, Target, with, _}, Context}) ->
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
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

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
