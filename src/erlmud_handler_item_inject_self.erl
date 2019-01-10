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
-module(erlmud_handler_item_inject_self).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%attempt({_Owner, Props, {Action, Obj, ItemName, BodyPart}})

attempt({_Owner, Props, Message = {Object, Action, ItemName}})
  when is_binary(ItemName) andalso
       Action == get; Action == drop ->
    case is_name(Props, ItemName) of
        true ->
            NewMessage = {Object, Action, self()},
            log([{type, inject_self},
                 {object, self()},
                 {action, Action},
                 {name, ItemName},
                 {props, Props},
                 {message, Message},
                 {resend, NewMessage},
                 {sub, true}]),
            %% TODO why sub if we're not listening for success?
            %% resending to source is kind of arbitrary:
            %% Target might have initiated the move.
            Result = {resend, Object, NewMessage},
            {Result, true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt({_Owner, Props, Message = {ItemName, move, from, Source, to, Target}})
  when is_binary(ItemName) ->
    case is_name(Props, ItemName) of
        true ->
            NewMessage = {self(), move, from, Source, to, Target},
            log([{type, inject_self},
                 {sub_type, move},
                 {object, self()},
                 {name, ItemName},
                 {props, Props},
                 {message, Message},
                 {resend, NewMessage},
                 {sub, true}]),
            %% TODO again, why sub if we're not listening for success?
            %% send to self() since we _know_ that's a PID.
            %% Source might be a binary name
            Result = {resend, self(), NewMessage},
            {Result, true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_name(Props, Name) ->
    ItemName = proplists:get_value(name, Props, ""),
    match == re:run(ItemName, Name, [{capture, none}]).

log(Props) ->
    erlmud_event_log:log(debug, [{module, ?MODULE} | Props]).
