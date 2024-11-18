%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_resource_reserve).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

% If something reserves us and we have the same owner (character).
attempt({#{owner := Owner},
         Props,
         {Owner, reserve, Amount,
          'of', Self,
          for, Proc,
          for, _Purpose,
          _Times, times}, _})
  when Self == self() ->
    Log = [{?SOURCE, Owner},
           {?EVENT, reserve},
           {amount, Amount},
           {?TARGET, Self},
           {for, Proc}],
    ?SUCCEED_SUB;
attempt({#{owner := Owner}, Props, {Owner, unreserve, Self, for, Proc}, _})
  when Self == self() ->
    Log = [{?SOURCE, Owner},
           {?EVENT, unreserve},
           {?TARGET, Self},
           {for, Proc}],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Self, update_tick}, _}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, update_tick}],
    ?SUCCEED_NOSUB;

attempt(_) ->
    undefined.

succeed({Props,
         {Character, reserve, Amount,
          'of', Self,
          for, Proc,
          for, Purpose,
          Times0, times}, _})
  when Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, reserve},
           {amount, Amount},
           {?TARGET, Self},
           ?RULES_MOD,
           {for, Proc}],
    Times = maybe_fix_times(Times0),
    Reservations = proplists:get_value(reservations, Props, []),
    %Props2 = case lists:member({Proc, Amount}, Reservations) of
    Props2 = case has_reservation(Proc, Amount, Reservations) of
                 true ->
                     Props;
                 false ->
                     lists:keystore(reservations,
                                    1,
                                    Props,
                                    {reservations, [{Proc, {Amount, Purpose, Times}} | Reservations]})
                    %[{reservations, [{Proc, {Amount, Times}} | Reservations]} | proplists:delete(reservations, Props)]
             end,
    Props3 = update_tick(Props2),
    {Props3, Log};
succeed({Props, {Character, unreserve, Self, for, Proc}, _})
  when Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, unreserve},
           {?TARGET, Self},
           ?RULES_MOD,
           {for, Proc}],
    Reservations = proplists:get_value(reservations, Props, []),
    NewReservations = lists:keydelete(Proc, 1, Reservations),
    Props2 = lists:keystore(reservations,
                            1,
                            Props,
                            {reservations, NewReservations}),
    Props3 = update_tick(Props2),
    {Props3, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

update_tick(Props) ->
    Self = self(),
    Reservations = proplists:get_value(reservations, Props, []),
    Tick = proplists:get_value(tick, Props, undefined),
    PerTick = proplists:get_value(per_tick, Props, 1),
    case {Reservations, Tick} of
        {[_ | _], undefined} ->
            Ref = make_ref(),
            egre_object:attempt(Self, {Self, tick, Ref, with, PerTick}),
            [{tick, Ref} | Props];
        {[], _} ->
            lists:keydelete(tick, 1, Props);
        {Res, Tick} ->
            ct:pal("~p update tick 'fell through' with Reservations ~p and Tick ~p",
                   [self(), Res, Tick]),
            Props
    end.

has_reservation(Proc, Amount, Reservations) ->
    lists:any(fun({Proc_, {Amount_, _, _}})
                    when Proc_ == Proc,
                         Amount_ == Amount ->
                      true;
                 (_) ->
                      false end,
              Reservations).

maybe_fix_times(infinite) ->
    io:format("*** Reservation with 'infinite' times: use 'infinity' times. ***"),
    infinity;
maybe_fix_times(Times) ->
    Times.
