%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_handler_attack).
-behaviour(erlmud_handler).

%% This generic handler is added to anything that attacks, because
%% each attacking item will control reserving and being allocated
%% resources on its own. That is, there isn't a single attack process
%% for the character that manages all the different attack vectors.
%% Each attack vector registers for resource allocation and fires off
%% attacks whenever all of its resource needs are fulfilled.

-include("include/erlmud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%% This process' parent character is attacking a target
attempt({#parents{character = Character}, Props, {Character, attack, _Target}}) ->
    {succeed, true, Props};

attempt({#parents{character = Character}, Props, {Character, attack, _Target, with, Self}})
  when Self == self() ->
    {succeed, true, Props};

%% This process landed an attack on a target
attempt({#parents{}, Props, {_Character, calc, _Hit, on, _Target, with, Self}})
  when Self == self() ->
    {succeed, true, Props};

%% This process needs to calculate damage to a target
attempt({#parents{}, Props, {_Character, calc, _Damage, to, _Target, with, Self}})
  when Self == self() ->
    {succeed, true, Props};

%% This process did damage to a target
attempt({#parents{}, Props, {_Character, does, _Damage, to, Target, with, Self}}) when Self == self(), is_pid(Target) ->
    {succeed, true, Props};

%% All processes belonging to this character need to stop attacking
attempt({#parents{character = Character}, Props, {Character, stop_attacking, _Target}}) ->
    {succeed, true, Props};

attempt(_) ->
    undefined.

%% An attack by our character has been successfully instigated but with no
%% specfic attack vector: we'll kick off an attempt to attack with this item
%% specifically.
succeed({Props, {Character, attack, Target}}) when is_pid(Target) ->
    erlmud_object:attempt(self(), {Character, attack, Target, with, self()}),
    Props;

%% An attack by our character has been successfully instigated using this process:
%% we'll register for resources and implement the attack when we have them.
succeed({Props, {Character, attack, Target, with, _Self}}) ->
    reserve(Character, proplists:get_value(resources, Props, [])),
    lists:keystore(target, 1, Props, {target, Target});

succeed({Props, {Character, calc, Hit, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       Hit > 0 ->
    erlmud_object:attempt(self(), {Character, calc, _InitialDamage = 0, to, Target, with, Self}),
    Props;
succeed({Props, {_Character, calc, _Miss, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    % TODO: say "you missed!"
    Props;
succeed({Props, {Character, calc, Damage, to, Target, with, Self}})
  when Self == self(),
       Damage > 0 ->
    erlmud_object:attempt(self(), {Character, does, Damage, to, Target, with, Self}),
    Props;
succeed({Props, {_Character, calc, _NoDamage, to, _Target, with, Self}})
  when Self == self() ->
    %% Attack failed (No damage was done)
    %% TODO: output something to the client like
    %% "You manage to hit <target> but fail to do any damage"
    %%       _if_ this is a player
    Props;
succeed({Props, {Character, stop_attack}}) ->
    unreserve(Character, Props),
    [{is_attacking, false} | Props];

succeed({Props, Msg}) ->
    log([<<"saw ">>, Msg, <<" succeed">>]),
    Props.

fail({Props, target_is_dead, _Message}) ->
    log([<<"Stopping because target is dead">>]),
    erlmud_object:attempt(self(), {self(), stop_attack}),
    Props;
fail({Props, _Reason, _Message}) ->
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [?MODULE | Terms]).

unreserve(Owner, Props) ->
    reserve_op(unreserve, Owner, Props).

reserve(Owner, Props) ->
    reserve_op(reserve, Owner, Props).

reserve_op(Op, Character, Props) when is_list(Props) ->
    [reserve_op(Op, Character, R) || R <- proplists:get_value(resources, Props, [])];

reserve_op(Op, Character, Resource) ->
    erlmud_object:attempt(self(), {Character, Op, Resource, for, self()}).
