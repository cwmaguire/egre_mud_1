%% Copyright (c) 2016, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_handler_attack_resource).
-behaviour(erlmud_handler).

%% respond to resources being added to the owner by reserving
%% those resources and kicking off attacks
%%
%% 1) subscribe to resource increase
%% 2) on resource increase success kick off resource reservation
%% 3) on resource reservation success allocate resources
%% 4) if any attack has all the necessary resources then kick off attack
%% 5) on stop_attack unreserve resources

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

attempt({#parents{character = Character,
                  owner = Owner},
         Props,
         {Character, attack, _Target, with, Owner}}) ->
    {succeed, true, Props};

attempt({#parents{character = Character},
         Props,
         {move, Character, _From, _To, _Exit}}) ->
    {succeed, true, Props};

attempt({#parents{character = Character},
         Props,
         {die, Character}}) ->
    {succeed, true, Props};

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, attack, _Target, with, Owner}}) ->
    [reserve(Character, Resource, Amount, Owner) || {resource, Resource, Amount} <- Props],
    Props;

succeed({Props, {Character, move, _From, _To, _Exit}}) ->
    Owner = proplists:get_value(owner, Props),
    [unreserve(Character, Resource, Owner) || {resource, Resource} <- Props],
    Props;

succeed({Props, {die, Character}}) ->
    Owner = proplists:get_value(owner, Props),
    unreserve(Character, Owner, Props),
    Props;

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

reserve(Character, Resource, Amount, Owner) ->
    erlmud_object:attempt(self(), {Character, reserve, Amount, 'of', Resource, for, Owner}).

unreserve(Character, Owner, Props) when is_list(Props) ->
    [unreserve(Character, Resource, Owner) || {resource, Resource} <- Props];

unreserve(Character, Resource, Owner) ->
    erlmud_object:attempt(self(), {Character, unreserve, Resource, for, Owner}).

%log(Level, IoData) ->
    %erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).