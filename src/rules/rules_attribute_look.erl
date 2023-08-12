%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_handler_attribute_look).
-behaviour(egre_handler).
-compile({parse_transform, egre_protocol_parse_transform}).

-include_lib("egre/include/egre.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {Source, describe, Owner, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Owner},
           {context, Context}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Source, describe, Self, with, Context}}) when Self == self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Self},
           {context, Context}],
    Props2 = describe(Source, Props, Context, deep),
    {Props2, Log};
succeed({Props, {Source, describe, Target, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Target},
           {context, Context}],
    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, Context, shallow);
            _ ->
                ok
        end,
    {Props, Log};
succeed({Props, _Msg}) ->
    {Props, _Log = []}.

-spec fail({proplist(), any(), tuple()}) -> {proplist(), proplist()}.
fail({Props, _Reason, _Msg}) ->
    {Props, _Log = []}.

describe(Source, Props, Context, shallow) ->
    send_description(Source, Props, Context);
describe(Source, Props, Context, deep) ->
    send_description(Source, Props, Context),
    Name = proplists:get_value(name, Props),
    NewContext = <<Context/binary, Name/binary, " -> ">>,
    egre_object:attempt(Source, {Source, describe, self(), with, NewContext}).

send_description(Source, Props, Context) ->
    Description = description(Props),
    egre_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.

description(Props) when is_list(Props) ->
    Type = proplists:get_value(type, Props),
    DescTemplate = egre_config:desc_template(Type),
    log([{desc_template, DescTemplate},
         {desc_type, Type},
         {object, self()},
         {handler, ?MODULE},
         {target, self()}
         | egre_event_log:flatten(Props)]),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    RawText;
description_part(Props, DescProp) ->
    log([{attribute_desc_part, DescProp},
         {object, self()},
         {handler, ?MODULE},
         {target, self()}
         | egre_event_log:flatten(Props)]),
    prop_description(proplists:get_value(DescProp,
                                         Props,
                                         <<"?? !",
                                           (atom_to_binary(DescProp, utf8))/binary,
                                           " ??">>)).

prop_description(undefined) ->
    [];
prop_description(Int) when is_integer(Int) ->
    integer_to_binary(Int);
prop_description(Value) when not is_pid(Value) ->
    Value.

log(Proplist) ->
    egre_event_log:log(debug, [{module, ?MODULE} | Proplist]).
