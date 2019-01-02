%% Copyright (c) 2017, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(erlmud_handler_resource_reserve).
-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

% If something reserves us and we have the same owner (character).
attempt({#parents{owner = Owner}, Props, {Owner, reserve, _Amount, 'of', Self, for, _Proc}})
  when Self == self() ->
    {succeed, true, Props};
attempt({#parents{owner = Owner}, Props, {Owner, unreserve, Self, for, _Proc}})
  when Self == self() ->
    {succeed, true, Props};
attempt({#parents{}, Props, {Self, update_tick}}) when Self == self() ->
    {succeed, false, Props};

attempt(_) ->
    undefined.

succeed({Props, {_Character, reserve, Amount, 'of', Self, for, Proc}})
  when Self == self() ->
    log(debug, [<<"Reserving ">>, Amount, <<" of ">>, Self, <<" for ">>, Proc, <<"\n">>]),
    Reservations = proplists:get_value(reservations, Props, []),
    Props2 = case lists:member({Proc, Amount}, Reservations) of
                 true ->
                     Props;
                 false ->
                    [{reservations, Reservations ++ [{Proc, Amount}]} | proplists:delete(reservations, Props)]
             end,
    %io:format("New props (with reservations): ~p~n", [Props2]),
    update_tick(Props2);
succeed({Props, {_Character, unreserve, Self, for, Proc}})
  when Self == self() ->
    Reservations = proplists:get_value(reservations, Props, []),
    Props2 = lists:keystore(reservations, 1, Props, {reservations, lists:keydelete(Proc, 1, Reservations)}),
    update_tick(Props2);
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).

update_tick(Props) ->
    Self = self(),
    log(debug, [Self, <<" updating tick">>, <<"\n">>]),
    Reservations = proplists:get_value(reservations, Props, []),
    Tick = proplists:get_value(tick, Props, undefined),
    PerTick = proplists:get_value(per_tick, Props, 1),
    case {Reservations, Tick} of
        {[_ | _], undefined} ->
            log(debug, [Self, <<" creating new tick">>, <<"\n">>]),
            Ref = make_ref(),
            erlmud_object:attempt(Self, {Self, tick, Ref, with, PerTick}),
            [{tick, Ref} | Props];
        {[], _} ->
            log(debug, [Self, <<" deleting tick">>, <<"\n">>]),
            lists:keydelete(tick, 1, Props);
        _ ->
            Props
    end.