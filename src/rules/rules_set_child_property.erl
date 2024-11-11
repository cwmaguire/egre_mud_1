%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_set_child_property).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

%% @doc Only if the message has our owner do we set the character and
%% then propagate the message. Otherwise we are not a child of the
%% source process and the message shouldn't go any further, so we fail
%% it. Nothing should subscribe to the message.
%%
%% This should cause a cascade of messages that keep starting at the current
%% child and going out to children until it runs out of children.
%%
%% If we move an item from one character to another, one body_part to another
%% or one item to another then we might get a "clear" and "set" out of order.
%% If we're going to clear out a previous character, body_part or owner we
%% need to specify what value we're clearing out. If that value is already
%% set to something else then we should leave it as is.

-include("mud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner},
         Props,
         {Owner, set_child_property, Key, Value},
         _}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, set_child_property},
           {?TARGET, self()},
           {key, Key},
           {value, Value}],
    NewEvent = {self(), set_child_property, Key, Value},
    #result{result = {broadcast, NewEvent},
            subscribe = false,
            props = lists:keystore(Key, 1, Props, {Key, Value}),
            log = Log};
attempt({#{owner := Owner},
         Props,
         {Owner, set_child_properties, ParentProps},
         _}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, set_child_properties},
           {?TARGET, self()}],
    NewEvent = {self(), set_child_properties, ParentProps},
    #result{result = {broadcast, NewEvent},
            subscribe = false,
            props = lists:foldl(fun apply_parent_prop/2, Props, ParentProps),
            log = Log};
attempt({#{owner := Owner},
         Props,
         {Owner, clear_child_property, Key = top_item,
          'if', TopItem = #top_item{item = Item, ref = Ref}},
         _}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, clear_child_property},
           {key, Key}],
    NewEvent = {self(), clear_child_property, top_item, 'if', TopItem},
    %% Only clear the top item if our #top_item{} has the same ref.
    %% Otherwise another item (or this same item) may have already
    %% set our top_item to itself
    %% e.g. self() goes from Parent->NotParent->Parent
    %% In which case will unset and reset itself as the top item, but maybe
    %% not always in the right order (because of graph traversal)
    Props2 = case proplists:get_value(top_item, Props) of
                 #top_item{item = Item, ref = Ref} ->
                     lists:keydelete(top_item, 1, Props);
                 _ ->
                     Props
             end,
    #result{result = {broadcast, NewEvent},
            subscribe = false,
            props = Props2,
            log = Log};
attempt({#{owner := Owner},
         Props,
         {Owner, clear_child_property, Key, 'if', Value},
         _}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, clear_child_property},
           {key, Key},
           {value, Value}],
    NewEvent = {self(), clear_child_property, Key, 'if', Value},
    Props2 = case proplists:get_value(Key, Props) of
                 Value ->
                     lists:keydelete(Key, 1, Props);
                 _ ->
                     Props
             end,
    #result{result = {broadcast, NewEvent},
            subscribe = false,
            props = Props2,
            log = Log};
attempt({_, Props, {_, set_child_property, _, _}, _}) ->
    Log = [{?EVENT, set_child_property}],
    {{fail, not_a_child}, _Subscribe = false, Props, Log};
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

apply_parent_prop({K, V}, Props) ->
    lists:keystore(K, 1, Props, {K, V}).
