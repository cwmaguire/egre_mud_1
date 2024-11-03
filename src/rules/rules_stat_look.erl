%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_stat_look).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner},
         Props,
         {Source, look, Owner, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, look},
           {?TARGET, Owner},
           {context, Context}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Source, look, Target, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, look},
           {?TARGET, Target},
           {context, Context}],

    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, Context);
            _ ->
                ok
        end,
    {Props, Log};
succeed({Props, _Msg}) ->
    Props.

fail({Props, _Result, _Msg}) ->
    Props.

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.

describe(Source, Props, Context) ->
    Description = description(Props),
    egre_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

description(Props) when is_list(Props) ->
    DescTemplate = application:get_env(egre, item_desc_template, []),
    log([<<"item desc template: ">>, DescTemplate]),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    RawText;
description_part(Props, DescProp) ->
    log([<<"body part description_part DescProp: ">>, DescProp, <<" from Props: ">>, Props]),
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

log(Terms) ->
    log(debug, Terms).

log(Level, Terms) ->
    egre_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
