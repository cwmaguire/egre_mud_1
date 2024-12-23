%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_resource_tick).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

%% This is a tick handler for a resource process. Resource processes manage
%% how often other processes can perform actions such as attacks. Resource
%% processes gain more resources to allocate with each tick. When a tick
%% event occurs the resource will allocate that resource to the next
%% process or processes in line depending on how much each next successive
%% process needs.

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Self, tick, Ref, with, Count}, _})
  when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, tick},
           {ref, Ref},
           {count, Count}],
    case proplists:get_value(tick, Props, undefined) of
        Ref ->
            ?SUCCEED_SUB;
        _ ->
            ?FAIL_NOSUB(<<"unmatched (stale?) resource tick">>)
    end;

attempt(_) ->
    undefined.

succeed({Props, {Self, tick, Ref, with, Count}, _})
  when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, tick},
           ?RULES_MOD,
           {ref, Ref},
           {count, Count}],
    Current = proplists:get_value(current, Props, 0),
    Max = proplists:get_value(max, Props, 0),
    New = min(Count + Current, Max),
    PerTick = proplists:get_value(per_tick, Props, 1),
    TickTime = proplists:get_value(tick_time, Props, _OneSecond = 1000),
    Reservations = proplists:get_value(reservations, Props, []),
    {Props2, RotatedReservations, Remaining} =
        case {Reservations, New} of
            {[], Max} ->
                PropsWithoutTick = lists:keydelete(tick, 1, Props),
                {PropsWithoutTick, [], Max};
            _ ->
                %% For now just make each tick take at _least_ PerTick
                %% millis instead of trying to wait close to a PerTick,
                %% or trying to correct for a long previous tick.
                egre:attempt_after(TickTime,
                                   Self,
                                   {Self, tick, Ref, with, PerTick}),
                Type = proplists:get_value(type, Props),
                {Reservations2, StillRemaining} =
                    allocate(Type, Reservations, New),
                {Props, Reservations2, StillRemaining}
        end,
    Props3 = lists:keydelete(current, 1, Props2),
    Props4 = lists:keydelete(reservations, 1, Props3),
    Props5 = [{current, Remaining},
              {reservations, RotatedReservations} | Props4],
    {Props5, Log};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

allocate(Type, [{Proc, {Required, Purpose, Times}} | Reservations], Available)
  when Available >= Required,
       (Times == infinity orelse
       (is_integer(Times) andalso Times > 0)) ->
    egre_object:attempt(Proc,
                        {self(), allocate,
                         Required, 'of', Type,
                         to, Proc,
                         for, Purpose}),
    TimesLeft = times_left(Times),
    RotatedReservations = Reservations ++ [{Proc, {Required, Purpose, TimesLeft}}],
    allocate(Type, RotatedReservations, Available - Required);
allocate(Type, [{_Proc, {_Required, _Purpose, Times}} | Reservations], Available)
  when is_integer(Times),
       Times =< 0 ->
    allocate(Type, Reservations, Available);
allocate(_, Reservations, Available) ->
    {Reservations, Available}.

times_left(infinity) ->
    infinity;
times_left(Int) when is_integer(Int) ->
    Int - 1.
