%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_quest).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{}, Props, {Self, quest, Quest}})
  when Self == self() ->
    Log = [{?EVENT, quest_registration},
           {?SOURCE, Quest},
           {?TARGET, self()}],
    {succeed, _ShouldSubscribe = true, Props, Log};
attempt({#parents{owner = Room},
         Props,
         {Player, _PlayerName, says, <<"Quest please!">>, in, Room}})
  when Player /= self() ->
    Log = [{?EVENT, says},
           {?SOURCE, self()},
           {?TARGET, Room}],
    MaybePlayerQuest = proplists:get_value(player_quest, Props),
    ShouldSub =
        case MaybePlayerQuest of
           undefined ->
                false;
           _ ->
                true
        end,
    {succeed, ShouldSub, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {_Self, quest, Quest}}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, quest_registration},
           {?TARGET, self()}],
    egre:attempt(Quest, {self(), quest, Quest, ack}, false),
    {[{quest, Quest} | Props], Log};
succeed({Props, {Player, _PlayerName, says, Phrase, in, _Room}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, say},
           {?TARGET, self()}],
    maybe_start_quest(Player, Phrase, Props),
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

maybe_start_quest(Player, Phrase, Props) ->
    case proplists:get_value(player_quest, Props) of
        Quest when is_list(Quest) ->
            Trigger = proplists:get_value(trigger, Quest),
            case Trigger of
                {say, Phrase} ->
                    QuestProps = proplists:get_value(props, Quest),
                    start_quest(Player, QuestProps);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

start_quest(Player, QuestProps) ->
    NewQuestProps = [{owner, Player} | QuestProps],
    supervisor:start_child(egre_object_sup, [p_quest, NewQuestProps]),
    egre:attempt(Player, {send, Player, <<"You've received a quest!">>}).
