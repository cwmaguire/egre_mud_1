%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_item_look).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include_lib("egre/include/egre.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{}, Props, {Source, look, Self}}) when Self == self() ->
    log([{?EVENT, look},
         {?SOURCE, Source} ]),
    {succeed, true, Props};
attempt({#parents{owner = Owner},
         Props,
         {Source, describe, Owner, with, Context}}) ->
    log([{?EVENT, describe},
         {?SOURCE, Source},
         {?TARGET, Owner},
         {context, Context}]),
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {Source, look, Self}}) ->
    log([{?EVENT, look},
         {object, Self},
         {?SOURCE, Source},
         {?TARGET, Self}]),
    describe(Source, Props, <<>>, deep),
    Props;
succeed({Props, {Source, describe, Target, with, Context}}) ->
    _ = case is_owner(Target, Props) of
            true ->
                log([{?EVENT, describe},
                     {?SOURCE, Source},
                     {?TARGET, self()},
                     {context, Context}]),
                describe(Source, Props, Context, shallow);
            _ ->
                ok
        end,
    Props;
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
    DescTemplate = mud_config:desc_template(item),
    log([{desc_template, DescTemplate},
         {object, self()},
         {handler, ?MODULE},
         {target, self()}
         | egre_event_log:flatten(Props)]),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    RawText;
description_part(Props, DescProp) ->
    log([{item_desc_part, DescProp},
         {object, self()},
         {handler, ?MODULE},
         {target, self()},
         {props, Props}]),
    prop_description(proplists:get_value(DescProp,
                                         Props,
                                         <<"?? !",
                                           (atom_to_binary(DescProp, utf8))/binary,
                                           " ??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

log(Terms) ->
    egre_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
