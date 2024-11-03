%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_util).

-include("mud.hrl").

-export([atob/1]).
-export([itob/1]).
-export([describe/2]).
-export([extract_from_props/1]).
-export([serialize/2]).

atob(Atom) ->
    list_to_binary(atom_to_list(Atom)).

itob(Int) ->
    list_to_binary(integer_to_list(Int)).

describe(Template, Props) ->
    DescTemplate = mud_config:desc_template(Template),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    RawText;
description_part(Props, DescProp) ->
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

%% TODO not used?
prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

extract_from_props(Props) ->
    Owner = proplists:get_value(owner, Props),
    Character = proplists:get_value(character, Props),
    TopItem = proplists:get_value(top_item, Props),
    BodyPart = proplists:get_value(body_part, Props),
    %Parents = #parents{owner = Owner,
                       %character = Character,
                       %top_item = TopItem,
                       %body_part = BodyPart},

    KeyFromElem = fun({K, _}) -> K end,
    ValFromElem = fun({_, V}) -> V end,
    GroupedItems = maps:groups_from_list(KeyFromElem, ValFromElem, Props),
    FirstEventParam = maps:map(fun unwrap_single_vals/2, GroupedItems),

    LogProps = [{owner, Owner},
                {character, Character},
                {top_item, TopItem},
                {body_part, BodyPart}],

    {FirstEventParam, LogProps}.


unwrap_single_vals(_, [V]) ->
    V;
unwrap_single_vals(_, Vs) ->
    Vs.

serialize(BodyPart = #body_part{}, JsonFun) ->
    body_part_to_binary(BodyPart, JsonFun);
serialize(TopItem = #top_item{}, JsonFun) ->
    top_item_to_binary(TopItem, JsonFun);
serialize(Value, _) ->
    Value.

body_part_to_binary(#body_part{body_part = BodyPart,
                               type = Type,
                               ref = Ref},
                    JsonFun) ->
    [<<"#body_part{bp = ">>,
     JsonFun(BodyPart),
     <<", type = ">>,
     JsonFun(Type),
     <<", ref = ">>,
     JsonFun(Ref),
     <<"}">>].

top_item_to_binary(#top_item{item = Item,
                             is_active = IsActive,
                             is_wielded = IsWielded,
                             ref = Ref},
                   JsonFun) ->
    [<<"#top_item{item = ">>,
     JsonFun(Item),
     <<", active? = ">>,
     JsonFun(IsActive),
     <<", wielded? = ">>,
     JsonFun(IsWielded),
     <<", ref = ">>,
     JsonFun(Ref),
     <<"}">>].
