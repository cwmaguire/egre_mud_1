%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_stop).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

%% @doc stop and re-broadcast

-include_lib("egre/include/egre.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{character = Character}, Props, {stop, Character}}) ->
    Log = [{?SOURCE, Character},
           {?TARGET, self()},
           {room, self()},
           {?EVENT, stop}],
    ct:pal("stop (~p) got attempt {stop, ~p}~n", [self(), Character]),
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, Msg = {stop, Character}}) ->
    Log = [{?SOURCE, Character},
           {?TARGET, self()},
           {room, self()},
           {?EVENT, stop}],
    ct:pal("stop (~p) got succeed for ~p~n", [self(), Msg]),
    case proplists:get_value(drop_on_death, Props, false) of
        true ->
            ct:pal("Not stopping (~p) because drop_on_death is true~n", [self()]),
            {Props, Log};
        _ ->
            {stop, finished, Props, Log}
    end;
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
