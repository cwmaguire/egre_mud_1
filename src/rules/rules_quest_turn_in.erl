-module(rules_quest_turn_in).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Player,
           giver := Giver,
           is_complete := true},
         Props,
         {Player, quests, from, Giver, turn, in},
         _}) ->
    Log = [{?EVENT, turn_in_quest},
           {?SOURCE, Giver},
           {?TARGET, self()}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Player, quests, from, _Giver, turn, in}, _}) ->
    Log = [{?EVENT, give_rewards},
           {?SOURCE, self()},
           {?TARGET, Player}],
    Name = proplists:get_value(name, Props),
    Message = [<<"You have completed the the quest \"">>, Name, <<"\"!">>],
    egre:attempt(Player, {send, Player, Message}),

    %% Give rewards
    [give_reward(Player, Reward) || Reward <- proplists:get_value(rewards, Props)],
    Props2 = [{is_turned_in, true} | Props],
    {Props2, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

give_reward(Player, {event, Event}) ->
    RenderedEvent = render_event(Event, Player),
    egre:attempt(Player, RenderedEvent).

render_event(Event, Player) ->
    List = tuple_to_list(Event),
    Rendered = lists:map(fun({player}) -> Player; (X) -> X end, List),
    list_to_tuple(Rendered).
