%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_quest_doer).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Self, quest, Quest}, _})
  when Self == self() ->
    Log = [{?EVENT, quest_registration},
           {?SOURCE, Quest},
           {?TARGET, self()},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{},
         Props,
         {Self, quests, _Active, _ReadyToTurnIn},
         _}) 
  when Self == self() ->
    Log = [{?EVENT, list_quests},
           {?SOURCE, self()},
           {?TARGET, self()},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{},
         Props,
         {Self, quests, all, _Active, _ReadyToTurnIn, _TurnedIn},
         _})
  when Self == self() ->
    Log = [{?EVENT, list_quests},
           {?SOURCE, self()},
           {?TARGET, self()},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {_Self, quest, Quest}, _}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, quest_registration},
           {?TARGET, self()},
           ?RULES_MOD],
    egre:attempt(Quest, {self(), quest, Quest, ack}, false),
    {[{quest, Quest} | Props], Log};

succeed({Props, {_Self, quests, Active, ReadyToTurnIn}, _}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, list_quests},
           {?TARGET, self()},
           ?RULES_MOD],
    send_quests(Active, ReadyToTurnIn, _TurnedIn = []),
    {Props, Log};

succeed({Props, {_Self, quests, all, Active, ReadyToTurnIn, TurnedIn}, _}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, list_quests},
           {?TARGET, self()},
           ?RULES_MOD],
    send_quests(Active, ReadyToTurnIn, TurnedIn),
    {Props, Log};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

send_quests(Active, ReadyToTurnIn, TurnedIn) ->
    ActiveIoList = maybe_io_list(<<"Active: ">>, Active),
    ReadyIoList = maybe_io_list(<<"Ready to turn in: ">>, ReadyToTurnIn),
    TurnedInIoList = maybe_io_list(<<"Turned in: ">>, TurnedIn),
    Msg =
        case {ActiveIoList, ReadyIoList, TurnedInIoList} of
            {<<>>, <<>>, <<>>} ->
                <<"You have no quests">>;
            _ ->
                [ActiveIoList, ReadyIoList, TurnedInIoList]
        end,
    egre:attempt(self(), {send, self(), Msg}).

maybe_io_list(_Prefix, []) ->
    <<>>;
maybe_io_list(Prefix, List) ->
    [Prefix, lists:join(<<", ">>, lists:sort(List)), <<"\n">>].
