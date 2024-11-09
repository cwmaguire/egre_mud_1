-module(rules_quest_exists).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Player,
           giver := QuestGiver,
           name := Name,
           is_turned_in := IsTurnedIn},
         Props,
         {QuestGiver, quest, Name, for, Player, _PlayerName}}) ->
    Log = [{?EVENT, quest_exists},
           {?SOURCE, QuestGiver},
           {?TARGET, Player}],
    case IsTurnedIn of
        true ->
            {{fail, turned_in}, _Sub = false, Props, Log};
        _ ->
            {{fail, in_progress}, _Sub = false, Props, Log}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

