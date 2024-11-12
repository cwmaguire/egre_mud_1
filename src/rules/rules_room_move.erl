%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_room_move).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{}, Props, {Char, move, from, Source, to, Target, via, Exit}, _})
  when Source == self(); Target == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Source},
           {to, Target},
           {exit, Exit}],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Char, enter_world, in, Self, with, Conn}, _}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, enter_world},
           {room, Self},
           {conn, Conn}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Char, move, from, Self, to, Target, via, Exit}, _}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Self},
           {to, Target},
           {exit, Exit}],
    Props2 = lists:keydelete(Char, 2, Props),
    {Props2, Log};
succeed({Props, {Char, move, from, Source, to, Self, via, Exit}, _}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Source},
           {to, Self},
           {exit, Exit}],
    egre_object:attempt(Self, {Char, look, Self}),
    Props2 = [{character, Char} | Props],
    {Props2, Log};
succeed({Props, {Char, enter_world, in, Self, with, Conn}, _}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, enter_world},
           {room, Self},
           {conn, Conn}],
    egre_object:attempt(Self, {Char, look, Self}),
    Props2 = [{character, Char} | Props],
    {Props2, Log};
succeed(_) ->
    undefined.

fail({Props, _Reason, {Char, move, from, Self, to, Target}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Self},
           {to, Target}],
    {Props, Log};
fail({Props, _Reason, {Char, move, from, Source, to, Target, via, Exit}}) when Source == self(); Target == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Source},
           {to, Target},
           {exit, Exit}],
    {Props, Log};
fail(_) ->
    undefined.
