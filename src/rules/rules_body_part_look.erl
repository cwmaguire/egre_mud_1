%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_body_part_look).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Source, look, Self}}) when Self == self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Self}],
    {succeed, true, Props, Log};
attempt({#{owner := Owner}, Props, {Source, describe, Owner, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Owner},
           {context, Context}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Source, look, Self}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Self}],
    describe(Source, Props, <<>>, deep),
    {Props, Log};
succeed({Props, {Source, describe, Target, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Target},
           {context, Context}],
    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, Context, deep);
            _ ->
                ok
        end,
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.

describe(Source, Props, Context, deep) ->
    send_description(Source, Props, Context),
    Name = proplists:get_value(name, Props),
    NewContext = <<Context/binary, Name/binary, " -> ">>,
    egre_object:attempt(Source, {Source, describe, self(), with, NewContext});
describe(Source, Props, Context, shallow) ->
    send_description(Source, Props, Context).

send_description(Source, Props, Context) ->
    Description = description(Props),
    egre_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

description(Props) when is_list(Props) ->
    DescTemplate = mud_config:desc_template(body_part),
    log([{?EVENT, body_part_desc}, {template, DescTemplate}]),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    log([{?EVENT, body_part_desc}, {desc_part, RawText}]),
    RawText;
description_part(Props, DescProp) ->
    log([{?EVENT, body_part_desc}, {desc_prop, DescProp}, {props, Props}]),
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

log(IoData) ->
    egre_event_log:log(debug, [{module, ?MODULE} | IoData]).
