%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_effect_inject_self).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {send, Dest, Msg, Placeholders}, _}) ->
    Log = [{?SOURCE, undefined},
           {?EVENT, send},
           {?TARGET, undefined}],
    case proplists:get_value(self(), Placeholders) of
        undefined ->
            ?SUCCEED_NOSUB;
        Placeholder ->
            Name = proplists:get_value(name, Props, <<"[no name]">>),
            NewMsg = binary:replace(Msg, Placeholder, Name),
            Event =
                case lists:keydelete(self(), 1, Placeholders) of
                    [] ->
                        {send, Dest, NewMsg};
                    NewPlaceholders ->
                        {send, Dest, NewMsg, NewPlaceholders}
                end,
            ?RESEND_NOSUB(self(), Event)
    end;
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
