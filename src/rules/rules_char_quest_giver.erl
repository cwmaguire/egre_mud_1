%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_quest_giver).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Room},
         Props,
         {Player, _PlayerName, says, <<"quests">>, in, Room}})
  when Player /= self() ->
    Log = [{?EVENT, says},
           {?SOURCE, self()},
           {?TARGET, Room}],
    Quests = proplists:get_all_values(player_quest, Props),
    ShouldSub =
        case Quests of
           [] ->
                false;
           _ ->
                true
        end,
    {succeed, ShouldSub, Props, Log};
attempt({#{owner := Room},
         Props,
         {Player, _PlayerName, says, <<"quest ", QuestName/binary>>, in, Room}})
  when Player /= self() ->
    Log = [{?EVENT, says},
           {?SOURCE, self()},
           {?TARGET, Room}],
    ShouldSub =
        case get_quest(QuestName, Props) of
           [] ->
                false;
           _ ->
                true
        end,
    {succeed, ShouldSub, Props, Log};
attempt({#{owner := Room},
         Props,
         {Player, _PlayerName, says, <<"quest turn in">>, in, Room}})
  when Player /= self() ->
    Log = [{?EVENT, say},
           {?SOURCE, self()},
           {?TARGET, Room}],
    {succeed, _ShouldSub = true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Player, PlayerName, says, <<"quests">>, in, _Room}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, say},
           {?TARGET, self()}],
    Quests = proplists:get_all_values(player_quest, Props),
    QuestNames = [proplists:get_value(name, Quest) || Quest <- Quests],
    egre:attempt(Player, {self(), quests, for, Player, PlayerName, QuestNames, _AlreadyActive = []}),
    {Props, Log};

succeed({Props, {Self, quests, for, Player, PlayerName, _Available = [], _Active = [_ | _]}}) ->
    Log = [{?SOURCE, Self},
           {?EVENT, no_available_quests},
           {?TARGET, Player}],
    egre:attempt(Player, {send, Player, <<"You already have all the quests ", PlayerName/binary>>}),
    {Props, Log};
succeed({Props, {Self, quests, for, Player, PlayerName, Available = [_ | _], _Active}}) ->
    Log = [{?SOURCE, Self},
           {?EVENT, available_quests},
           {?TARGET, Player}],
    Message = [<<"Available quests for ", PlayerName/binary, ": ">>, lists:join(<<", ">>, Available)],
    egre:attempt(Player, {send, Player, Message}),
    {Props, Log};
succeed({Props, {Self, quests, for, Player, PlayerName, _Available = [], _Active = []}}) ->
    Log = [{?SOURCE, Self},
           {?EVENT, no_quests},
           {?TARGET, Player}],
    Message = <<"I don't have any quests for you, ", PlayerName/binary>>,
    egre:attempt(Player, {send, Player, Message}),
    {Props, Log};

succeed({Props, {Player, _PlayerName, says, <<"quest turn in">>, in, _Room}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, say},
           {?TARGET, self()}],
    egre:attempt(Player, {Player, quests, from, self(), turn, in}, false),
    {Props, Log};

succeed({Props, {Player, PlayerName, says, _Phrase = <<"quest ", QuestName/binary>>, in, _Room}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, start_quest},
           {?TARGET, self()}],
    %% can succeed or fail
    egre:attempt(Player, {self(), quest, QuestName, for, Player, PlayerName}),
    {Props, Log};

succeed({Props, {Self, quest, QuestName, for, Player, PlayerName}}) ->
    Log = [{?SOURCE, Self},
           {?EVENT, no_quests},
           {?TARGET, Player}],
    Message = <<"Here is your quest, ", PlayerName/binary, ": ", QuestName/binary>>,
    egre:attempt(Player, {send, Player, Message}),
    maybe_start_quest(Player, QuestName, Props),
    {Props, Log};

succeed({Props, _}) ->
    Props.

fail({Props, completed, {Self, quest, QuestName, for, Player, PlayerName}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, start_quest},
           {?TARGET, Self}],
    Message = <<"You've already turned in ", QuestName/binary, ", ", PlayerName/binary>>,
    egre:attempt(Player, {send, Player, Message}),
    {Props, Log};
fail({Props, in_progress, {Self, quest, QuestName, for, Player, PlayerName}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, start_quest},
           {?TARGET, Self}],
    Message = <<"You're already working on ", QuestName/binary, ", ", PlayerName/binary>>,
    egre:attempt(Player, {send, Player, Message}),
    {Props, Log};
fail({Props, _, _}) ->
    Props.

maybe_start_quest(Player, QuestName, Props) ->
    case get_quest(QuestName, Props) of
        [Quest] ->
            start_quest(Player, Quest);
        _ ->
            ok
    end.

start_quest(Player, QuestProps) ->
    NewQuestProps = [{owner, Player}, {giver, self()} | QuestProps],
    {ok, Pid} = supervisor:start_child(egre_object_sup, [p_quest, NewQuestProps]),
    egre:attempt(Player, {send, Player, <<"You've received a quest!">>}),
    Pid.

get_quest(QuestName, Props) ->
    Quests = proplists:get_all_values(player_quest, Props),
    lists:filter(fun(Quest) -> proplists:get_value(name, Quest) == QuestName end, Quests).
