%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_body_part_inv).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([can_add/2]).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner},
         Props,
         {Item, move, from, Self, to, Owner},
         _})
  when Self == self(),
       is_pid(Item) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, Owner}],
    #result{result = succeed,
            subscribe = has_item_with_ref(Item, Props),
            props = Props,
            log = Log};
attempt({#{owner := Owner},
         Props,
         {Item, move, from, Owner, to, Self},
         _})
  when Self == self(),
       is_pid(Item) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Owner},
           {?TARGET, Self}],
    NewEvent = {Item, move, from, Owner, to, self(), limited, to, item_body_parts},
    Result = {resend, Owner, NewEvent},
    #result{result = Result,
            subscribe = true,
            props = Props,
            log = Log};
% We know both the target body part and the valid body parts for the item so
% we can see if this body part has space and if this body part matches the item.
attempt({#{owner := Owner},
         Props,
         {Item, move, from, Owner, to, Self, limited, to, ItemBodyParts},
         _})
  when Self == self(),
       is_pid(Item),
       is_list(ItemBodyParts) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Owner},
           {?TARGET, Self}],
    case can(add, Props, ItemBodyParts) of
        {false, Reason} ->
            #result{result = {fail, Reason},
                    subscribe = false,
                    props = Props,
                    log = [{limited, ItemBodyParts} | Log]};
        _ ->
            BodyPartType = proplists:get_value(body_part, Props, undefined),
            NewEvent = {Item, move, from, Owner, to, self(), on, body_part, type, BodyPartType},
            #result{result = {resend, Owner, NewEvent},
                    subscribe = true,
                    props = Props,
                    log = [{body_part_type, BodyPartType} | Log]}
    end;
%% The reason for "limited, to, item_body_parts" is that there are two conditions that have
%% to be met for an item to be added to a body part:
%% - the body part must have available space (e.g. an empty hand can hold a gun)
%% - the item must fit on that body part (e.g. an axe isn't going to be a hat)
%% This requires both the body part and the item each contribute to the message
%% before we can check if they are met. We add two placeholder flags to the message:
%% - 'first_available_body_part' if we don't know which part it will be yet
%% - 'limited', 'to', 'item_body_parts' if we don't know what body part types are valid
%%   for the body part.
attempt({#{owner := Owner},
         Props,
         {Item, move, from, Owner, to, first_available_body_part},
         _})
  when is_pid(Item) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Owner},
           {?TARGET, first_available_body_art}],
    NewEvent = {Item, move, from, Owner, to, first_available_body_part, limited, to, item_body_parts},
    #result{result = {resend, Owner, NewEvent},
            subscribe = true,
            props = Props,
            log = Log};

attempt({#{owner := Owner},
         Props,
         {Item, move, from, Owner, to, first_available_body_part, limited, to, ItemBodyParts},
         _})
  when is_pid(Item),
       is_list(ItemBodyParts) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Owner}],
    case can(add, Props, ItemBodyParts) of
        true ->
            BodyPartType = proplists:get_value(body_part, Props, undefined),
            Log2 = [{?TARGET, self()},
                    {body_part_type, BodyPartType}
                    | Log],
            NewEvent = {Item, move, from, Owner, to, self(), on, body_part, type, BodyPartType},
            #result{result = {resend, Owner, NewEvent},
                    subscribe = true,
                    props = Props,
                    log = Log2};
        _ ->
            Log2 = [{?TARGET, first_available_body_part},
                    {limited, ItemBodyParts}
                    | Log],
            #result{result = succeed,
                    subscribe = false,
                    props = Props,
                    log = Log2}
    end;
attempt({#{owner := Owner},
         Props,
         {Item, move, from, Owner, to, Self, on, body_part, type, BodyPartType},
         _})
  when Self == self() ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Owner},
           {?TARGET, Self},
           {body_part_type, BodyPartType}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Item, move, from, OldOwner, to, Self, on, body_part, type, BodyPartType}, _})
  when Self == self() ->
    Log = [{?EVENT, get_item},
           {item, Item},
           {?SOURCE, OldOwner},
           {?TARGET, Self},
           {body_part_type, BodyPartType}],
    ItemRef = make_ref(),
    egre_object:attempt(Item, {self(), set_child_property, body_part,
                                 #body_part{body_part = self(),
                                            type = BodyPartType,
                                            ref = ItemRef}}),
    {[{item, {Item, ItemRef}} | Props], Log};
succeed({Props, {Item, move, from, Self, to, NewOwner}, _})
  when Self == self() ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Self},
           {?TARGET, NewOwner}],
    Props2 = clear_child_body_part(Props, Item, NewOwner),
    {Props2, Log};
%% TODO I'm not sure if this gets used: _ItemBodyParts indicates this is an intermediate event
%% that should turn into a {BodyPart, BodyPartType} event
%succeed({Props, {move, Item, from, Self, to, NewOwner, _ItemBodyParts}})
  %when Self == self() ->
    %clear_child_body_part(Props, Item, NewOwner);
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

has_item_with_ref(Item, Props) ->
    case [Item_ || {item, {Item_, _Ref}} <- Props, Item_ == Item] of
        [_ | _] ->
            true;
        _ ->
            false
    end.

can(add, Props, ItemBodyParts) ->
    can_add(Props, ItemBodyParts);
can(remove, Props, Item) ->
    can_remove(Props, Item).

can_add(Props, ItemBodyParts) ->
    can_add([fun has_matching_body_part/2,
             fun has_space/2],
            Props,
            ItemBodyParts,
            true).

can_add([], _, _, Result) ->
    log([{?EVENT, can_add}, {result, Result}]),
    Result;
can_add(_, _, _, {false, Reason}) ->
    log([{?EVENT, can_add}, {result, false}, {reason, Reason}]),
    {false, Reason};
can_add([Fun | Funs], Props, ItemBodyParts, true) ->
    can_add(Funs, Props, ItemBodyParts, Fun(Props, ItemBodyParts)).

can_remove(_Props, _Item) ->
    true.

has_matching_body_part(Props, ItemBodyParts) ->
    BodyPart = proplists:get_value(body_part, Props, any),
    case {BodyPart, lists:member(BodyPart, ItemBodyParts)} of
        {any, _} ->
            true;
        {_, true} ->
            true;
        {_, _} ->
            {false, <<"Item is not compatible with body part">>}
    end.

has_space(Props, _) ->
    NumItems = length(proplists:get_all_values(item, Props)),
    MaxItems = proplists:get_value(max_items, Props, infinite),
    log([{?EVENT, has_space},
         {num_items, NumItems},
         {max_items, MaxItems}]),
    case proplists:get_value(max_items, Props, infinite) of
        infinite ->
            true;
        MaxItems when NumItems < MaxItems ->
            true;
        _ ->
            {false, <<"Body part is full">>}
    end.

clear_child_body_part(Props, Item, Target) ->
    log([{?EVENT, give_item},
         {to, Target},
         {props, Props}]),
    BodyPartType = proplists:get_value(body_part, Props, undefined),
    ItemRef = item_ref(Item, Props),
    egre_object:attempt(Item,
                             {Target,
                              clear_child_property,
                              body_part,
                              'if',
                              #body_part{body_part = self(),
                                         type = BodyPartType,
                                         ref = ItemRef}}),
    lists:keydelete({Item, ItemRef}, 2, Props).

item_ref(Item, Props) ->
    Items = proplists:get_all_values(item, Props),
    case [Ref || {Item_, Ref} <- Items, Item == Item_] of
        [] ->
            undefined;
        [Ref] ->
            Ref
    end.

log(IoData) ->
    egre_event_log:log(debug, [{module, ?MODULE} | IoData]).
