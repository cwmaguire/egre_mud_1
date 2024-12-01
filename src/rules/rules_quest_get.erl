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
attempt({#{owner := Player},
         Props,
         {Player, quests, Active, ReadyToTurnIn},
         _}) ->
    Log = [{?EVENT, list_quests},
           {?SOURCE, Player},
           {?TARGET, self()}],
    {Active2, ReadyToTurnIn2, _} = add_self(Props, Active, ReadyToTurnIn, []),
    NewEvent = {Player, quests, Active2, ReadyToTurnIn2},
    #result{result = succeed,
            subscribe = false,
            event = NewEvent,
            props = Props,
            log = Log};

attempt({#{owner := Player},
         Props,
         {Player, quests, all, Active, ReadyToTurnIn, TurnedIn},
         _}) ->
    Log = [{?EVENT, list_quests},
           {?SOURCE, Player},
           {?TARGET, self()}],
    {Active2, ReadyToTurnIn2, TurnedIn2} = add_self(Props, Active, ReadyToTurnIn, TurnedIn),
    NewEvent = {Player, quests, all, Active2, ReadyToTurnIn2, TurnedIn2},
    #result{result = succeed,
            subscribe = false,
            event = NewEvent,
            props = Props,
            log = Log};

attempt(_) ->
    undefined.

add_self(Props, Active, ReadyToTurnIn, TurnedIn) ->
    Name = proplists:get_value(name, Props),
    IsComplete = proplists:get_value(is_complete, Props),
    IsTurnedIn = proplists:get_value(is_turned_in, Props),
    case {IsComplete, IsTurnedIn} of
        {false, false} ->
            Active2 = [Name | Active],
            {Active2, ReadyToTurnIn, TurnedIn};
        {true, false} ->
            ReadyToTurnIn2 = [Name | ReadyToTurnIn],
            {Active, ReadyToTurnIn2, TurnedIn};
        {true, true} ->
            TurnedIn2 = [Name | TurnedIn],
            {Active, ReadyToTurnIn, TurnedIn2}
    end.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

