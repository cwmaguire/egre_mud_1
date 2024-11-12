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
         {Char, quests, for, Player, PlayerName, AvailableQuests, ActiveQuests},
         _}) ->
    Log = [{?EVENT, available_quests},
           {?SOURCE, Player},
           {?TARGET, self()}],
    RemainingQuests = lists:delete(Name, AvailableQuests),
    NewEvent = {Char, quests, for, Player, PlayerName, RemainingQuests, [Name | ActiveQuests]},
    #result{result = succeed,
            event = NewEvent,
            subscribe = false,
            props = Props,
            log = Log};
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

