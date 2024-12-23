
%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_inv).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

%% Injects the room, which might indicate this should be in ...room_inject_self,
%% except the character has a 'room' property, which is faster.
%% Also, characters can only be owned by rooms. This wouldn't work
%% for an item owned by a body part because an item might be owned by a
%% character, room or other item (e.g. container).
attempt({#{}, Props, {Self, Action, Item}, _Context})
  when Self == self() andalso
       is_pid(Item) andalso
       (Action == get orelse Action == drop) ->
    Log = [{?SOURCE, Self},
           {?EVENT, Action},
           {?TARGET, Item}],
    case Action == get orelse egre_object:has_pid(Props, Item) of
        true ->
            Room = proplists:get_value(owner, Props),
            {Source, Target} = case Action of
                drop ->
                    {Self, Room};
                get ->
                    {Room, Self}
            end,
            #result{result = {resend, Self, {Item, move, from, Source, to, Target}},
                    subscribe = true,
                    props = Props,
                    log = Log};
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{}, Props, {Item, move, from, Self, to, Room}, _})
  when Self == self(),
       is_pid(Item),
       is_pid(Room) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, Room}],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Item, move, from, Self, to, BodyPart, on, body_part, type, BodyPartType}, _})
  when Self == self() andalso
       is_pid(Item),
       is_pid(BodyPart) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, BodyPart},
           {body_part_type, BodyPartType}],
    ?SUCCEED_SUB;
%% TODO I suspect the name _Room means that it is expected that the source will be a room; is this so?
attempt({#{}, Props, {Item, move, from, Room, to, Self}, _})
  when Self == self() andalso
       is_pid(Item) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Room},
           {?TARGET, Self}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Item, move, from, Source, to, Self}, _}) when Self == self() ->
    Log = [{?EVENT, get_item},
           {?SOURCE, Source},
           {?TARGET, Self},
           ?RULES_MOD],
    egre_object:attempt(Item, {self(), set_child_property, character, self()}),
    {[{item, Item} | Props], Log};
succeed({Props, {Item, move, from, Self, to, BodyPart, on, body_part, type, BodyPartType}, _}) when Self == self() ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, BodyPart},
           {body_part_type, BodyPartType},
           ?RULES_MOD],
    Props2 = lists:keydelete(Item, 2, Props),
    {Props2, Log};
succeed({Props, {Item, move, from, Self, to, Target}, _}) when Self == self() ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, Target},
           ?RULES_MOD],
    Props2 = clear_child_character(Props, Item, Target),
    {Props2, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

clear_child_character(Props, Item, Target) ->
    log([{?EVENT, give_item}, {?SOURCE, self()}, {?TARGET, Target}, {props, Props}]),
    egre_object:attempt(Item, {Target, clear_child_property, character, 'if', self()}),
    lists:keydelete(Item, 2, Props).

log(Props) ->
    egre_event_log:log(debug, [{module, ?MODULE} | Props]).
