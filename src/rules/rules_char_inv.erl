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
attempt({_Owner, Props, {Self, Action, Item}})
  when Self == self() andalso
       is_pid(Item) andalso
       Action == get; Action == drop ->
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
            {{resend, Self, {Item, move, from, Source, to, Target}}, true, Props, Log};
        _ ->
            {succeed, _Interested = false, Props, Log}
    end;
attempt({_Owner, Props, {Item, move, from, Self, to, Room}})
  when Self == self() andalso
       is_pid(Item),
       is_pid(Room) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, Room}],
    {succeed, true, Props, Log};
attempt({_Owner, Props, {Item, move, from, Self, to, BodyPart, on, body_part, type, BodyPartType}})
  when Self == self() andalso
       is_pid(Item),
       is_pid(BodyPart) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, BodyPart},
           {body_part_type, BodyPartType}],
    {succeed, true, Props, Log};
%% TODO I suspect the name _Room means that it is expected that the source will be a room; is this so?
attempt({_Owner, Props, {Item, move, from, Room, to, Self}})
  when Self == self() andalso
       is_pid(Item) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Room},
           {?TARGET, Self}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Item, move, from, Source, to, Self}}) when Self == self() ->
    Log = [{?EVENT, get_item},
           {?SOURCE, Source},
           {?TARGET, Self}],
    egre_object:attempt(Item, {self(), set_child_property, character, self()}),
    {[{item, Item} | Props], Log};
succeed({Props, {Item, move, from, Self, to, BodyPart, on, body_part, type, BodyPartType}}) when Self == self() ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, BodyPart},
           {body_part_type, BodyPartType}],
    Props2 = lists:keydelete(Item, 2, Props),
    {Props2, Log};
succeed({Props, {Item, move, from, Self, to, Target}}) when Self == self() ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, Target}],
    Props2 = clear_child_character(Props, Item, Target),
    {Props2, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

clear_child_character(Props, Item, Target) ->
    log([{?EVENT, give_item}, {?SOURCE, self()}, {?TARGET, Target}, {props, Props}]),
    egre_object:attempt(Item, {Target, clear_child_property, character, 'if', self()}),
    lists:keydelete(Item, 2, Props).

log(Props) ->
    egre_event_log:log(debug, [{module, ?MODULE} | Props]).
