-module(rules_quest_get).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Player,
           giver := Char,
           name := Name},
         Props,
         {Char, quests, for, Player, AvailableQuests, ActiveQuests}}) ->
    Log = [{?EVENT, available_quests},
           {?SOURCE, Player},
           {?TARGET, self()}],

    RemainingQuests = lists:delete(Name, AvailableQuests),
    NewMessage = {quests, for, Player, RemainingQuests, [Name | ActiveQuests]},
    {succeed, NewMessage, false, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

