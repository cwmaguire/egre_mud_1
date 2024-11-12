%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_move).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Self, move, Direction}, _}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, move},
           {direction, Direction}],
    case proplists:get_value(owner, Props) of
        undefined ->
            #result{result = {fail, <<"Character doesn't have room">>},
                    subscribe = false,
                    props = Props,
                    log = Log};
        Room ->
            #result{result = {resend, Self, {Self, move, Direction, from, Room}},
                    subscribe = false,
                    props = Props,
                    log = [{from, Room} | Log]}
    end;
attempt({#{}, Props, {Self, move, Dir, from, From}, _}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, move},
           {direction, Dir},
           {from, From}],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Self, move, from, From, to, To, via, Exit}, _}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, move},
           {from, From},
           {to, To},
           {exit, Exit}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Self, move, from, Source, to, Target, via, Exit}, _}) when Self == self() ->
    Log = [{?EVENT, move},
           {?SOURCE, Self},
           {from, Source},
           {to, Target},
           {exit, Exit}],
    NewProps = set(room, Target, set(owner, Target, Props)),
    case proplists:get_value(is_attacking, Props) of
        true ->
            egre_object:attempt(self(), {self(), stop_attack});
        _ ->
            ok
    end,
    {NewProps, Log};
succeed({Props, {Self, move, Direction, from, Source}, _}) when Self == self(), is_atom(Direction) ->
    % rules_exit_move should have turned this into:
    % {Self, move, from, Source, to, Target, via, Exit}
    Log = [{?EVENT, move},
           {?SOURCE, Self},
           {direction, Direction},
           {from, Source}],
    % TODO Let the player know they didn't get anywhere: "There is no exit <Direction> here."
    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {Type, Obj}).
