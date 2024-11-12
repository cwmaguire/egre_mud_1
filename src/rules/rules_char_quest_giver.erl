%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_quest_giver).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{conn := _PlayerConnection}, _, _, _}) ->
    undefined;
attempt({#{owner := Room},
         Props,
         {Player, _PlayerName, says, <<"quests">>, in, Room}, _})
  when Player /= self() ->
    Log = [{?EVENT, say},
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
    #result{result = succeed,
            subscribe = ShouldSub,
            props = Props,
            log = Log};
attempt({#{owner := Room},
         Props,
         {Player, _PlayerName, says, <<"quest ", QuestName/binary>>, in, Room}, _})
  when Player /= self() ->
    Log = [{?EVENT, say},
           {?SOURCE, self()},
           {?TARGET, Room}],
    ShouldSub =
        case get_quest(QuestName, Props) of
           [] ->
                false;
           _ ->
                true
        end,
    #result{result = succeed,
            subscribe = ShouldSub,
            props = Props,
            log = Log};
attempt({#{owner := Room},
         Props,
         {Player, _PlayerName, says, <<"quest turn in">>, in, Room}, _})
  when Player /= self() ->
    Log = [{?EVENT, say},
           {?SOURCE, self()},
           {?TARGET, Room}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Player, PlayerName, says, <<"quests">>, in, _Room}, _}) when Player /= self() ->
    Log = [{?SOURCE, Player},
           {?EVENT, say},
           {?TARGET, self()},
           ?RULES_MOD],
    Quests = proplists:get_all_values(player_quest, Props),
    QuestNames = [proplists:get_value(name, Quest) || Quest <- Quests],
    egre:attempt(Player, {self(), quests, for, Player, PlayerName, QuestNames, _AlreadyActive = []}),
    {Props, Log};

succeed({Props, {Self, quests, for, Player, PlayerName, _Available = [], _Active = [_ | _]}, _}) when Self /= Player ->
    Log = [{?SOURCE, Self},
           {?EVENT, no_available_quests},
           {?TARGET, Player},
           ?RULES_MOD],
    Name = proplists:get_value(name, Props),
    egre:attempt(Player, {send, Player, <<Name/binary,
                                          " says: you already have all the quests, ",
                                          PlayerName/binary>>}),
    {Props, Log};
succeed({Props, {Self, quests, for, Player, PlayerName, Available = [_ | _], _Active}, _}) when Player /= Self ->
    Log = [{?SOURCE, Self},
           {?EVENT, available_quests},
           {?TARGET, Player},
           ?RULES_MOD],
    Name = proplists:get_value(name, Props),
    SortedAvailable = lists:sort(Available),
    Message = [<<Name/binary, " says: ",
               PlayerName/binary, ", I have these quests: ">>,
               lists:join(<<", ">>, SortedAvailable)],
    egre:attempt(Player, {send, Player, Message}),
    {Props, Log};
succeed({Props, {Self, quests, for, Player, PlayerName, _Available = [], _Active = []}, _}) when Player /= Self ->
    Log = [{?SOURCE, Self},
           {?EVENT, no_quests},
           {?TARGET, Player},
           ?RULES_MOD],
    Name = proplists:get_value(name, Props),
    Message = <<Name/binary, " says: I don't have any quests for you, ", PlayerName/binary>>,
    egre:attempt(Player, {send, Player, Message}),
    {Props, Log};

succeed({Props, {Player, _PlayerName, says, <<"quest turn in">>, in, _Room}, _}) when Player /= self() ->
    Log = [{?SOURCE, Player},
           {?EVENT, say},
           {?TARGET, self()},
           ?RULES_MOD],
    egre:attempt(Player, {Player, quests, from, self(), turn, in}, false),
    {Props, Log};

succeed({Props, {Player, PlayerName, says, _Phrase = <<"quest ", QuestName/binary>>, in, _Room}, _})
  when Player /= self() ->
    Log = [{?SOURCE, Player},
           {?EVENT, start_quest},
           {?TARGET, self()},
           ?RULES_MOD],
    %% can succeed or fail
    %egre:attempt(Player, {self(), quest, QuestName, for, Player, PlayerName}),
    Quests = proplists:get_all_values(player_quest, Props),
    QuestNames = [proplists:get_value(name, Quest) || Quest <- Quests],
    Name = proplists:get_value(name, Props),
    case lists:member(QuestName, QuestNames) of
        true ->
            egre:attempt(Player, {self(), quest, QuestName, for, Player, PlayerName});
        _ ->
            Message = <<Name/binary, " says: I don't have a quest called ", QuestName/binary, ", ", PlayerName/binary>>,
            egre:attempt(Player, {send, Player, Message})
    end,
    {Props, Log};

succeed({Props, {Self, quest, QuestName, for, Player, PlayerName}, _}) ->
    Log = [{?SOURCE, Self},
           {?EVENT, give_quest},
           {?TARGET, Player},
           ?RULES_MOD],
    Name = proplists:get_value(name, Props),
    Message = <<Name/binary, " says: Here is your quest, ", PlayerName/binary, ": ", QuestName/binary>>,
    egre:attempt(Player, {send, Player, Message}),
    maybe_start_quest(Player, QuestName, Props),
    {Props, Log};

succeed(_) ->
    undefined.

fail({Props, completed, {Self, quest, QuestName, for, Player, PlayerName}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, start_quest},
           {?TARGET, Self}],
    Name = proplists:get_value(name, Props),
    Message = <<Name/binary, " says: You've already turned in ", QuestName/binary, ", ", PlayerName/binary>>,
    egre:attempt(Player, {send, Player, Message}),
    {Props, Log};
fail({Props, in_progress, {Self, quest, QuestName, for, Player, PlayerName}, _}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, start_quest},
           {?TARGET, Self}],
    Name = proplists:get_value(name, Props),
    Message = <<Name/binary, " says: You're already working on ", QuestName/binary, ", ", PlayerName/binary>>,
    egre:attempt(Player, {send, Player, Message}),
    {Props, Log};
fail(_) ->
    undefined.

maybe_start_quest(Player, QuestName, Props) ->
    case get_quest(QuestName, Props) of
        [Quest] ->
            start_quest(Player, Quest);
        _ ->
            ok
    end.

start_quest(Player, QuestProps) ->
    NewQuestProps = [{owner, Player}, {giver, self()} | QuestProps],
    {ok, Pid} = supervisor:start_child(egre_object_sup, [undefined, NewQuestProps]),
    egre:attempt(Player, {send, Player, <<"You've received a quest!">>}),
    Pid.

get_quest(QuestName, Props) ->
    Quests = proplists:get_all_values(player_quest, Props),
    lists:filter(fun(Quest) -> proplists:get_value(name, Quest) == QuestName end, Quests).
