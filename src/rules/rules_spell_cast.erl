%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_spell_cast).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{character := Character},
         Props,
         {Character, cast, Self, on, Target},
         _})
  when Self == self(),
       is_pid(Target) ->
    Log = [{?SOURCE, Character},
           {?EVENT, cast},
           {?TARGET, Target}],
    ?SUCCEED_SUB;

attempt({#{}, Props, {Resource, allocate, Required, 'of', Type, to, Self, to, cast}, _})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Required},
           {resource_type, Type},
           {?SOURCE, Resource},
           {?TARGET, Self}],
    ?SUCCEED_SUB;

attempt(_) ->
    undefined.

succeed({Props, {Character, cast, _Self, on, Target}, _}) ->
    Log = [{?EVENT, cast},
           {?SOURCE, Character},
           {?TARGET, Target},
           {rules_module, spell_cast}],
    reserve(Character, Props, _Times = 1),
    %% XXX if you cast a spell at a second target, the first target will be wiped out
    %% maybe put them in a list and heal them in order
    Props2 = lists:keystore(target, 1, Props, {target, Target}),
    {Props2, Log};

succeed({Props, {Resource, allocate, Amt, 'of', Type, to, Self, to, cast}, _})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Amt},
           {resource_type, Type},
           {?SOURCE, Resource},
           {?TARGET, Self},
           {rules_module, spell_cast}],
    Allocated = update_allocated(Amt, Type, Props),
    Required = proplists:get_value(resources, Props, []),
    HasResources = has_resources(Allocated, Required),
    RemainingAllocated =
        case HasResources of
            true ->
                Character = proplists:get_value(character, Props),
                Target = proplists:get_value(target, Props),
                Event = {Character, affect, Target, because, Self},
                egre_object:attempt(self(), Event, false),
                deallocate(Allocated, Required);
            _ ->
                Allocated
        end,
    Props2 = lists:keystore(allocated_resources, 1, Props, {allocated_resources, RemainingAllocated}),
    {Props2, Log};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

reserve(Char, Props, Times) when is_list(Props) ->
    Resources = proplists:get_value(resources, Props, []),
    [reserve(Char, Res, Times, Amt) || {Res, Amt} <- Resources].

reserve(Character, Resource, Times, Amount) ->
    egre_object:attempt(self(),
                        {Character, reserve, Amount,
                         'of', Resource,
                         for, self(),
                         to, cast,
                         Times, times}).

update_allocated(New, Type, Props) ->
    Allocated = proplists:get_value(allocated_resources, Props, #{}),
    Curr = maps:get(Type, Allocated, 0),
    Allocated#{Type => Curr + New}.

has_resources(Allocated, Required) ->
    {_, AllocApplied} = lists:foldl(fun apply_resource/2, {Allocated, []}, Required),
    not lists:any(fun is_resource_lacking/1, AllocApplied).

apply_resource(_Resource = {Type, Required},
               {Allocated, Applied0}) ->
    AllocAmt = maps:get(Type, Allocated, 0),
    Applied1 = [{Type, Required - AllocAmt} | Applied0],
    {Allocated#{Type => 0}, Applied1}.

is_resource_lacking({_Type, Amount}) when Amount =< 0 ->
    false;
is_resource_lacking(_) ->
    true.

deallocate(Allocated, Required) ->
    lists:foldl(fun subtract_required/2, Allocated, Required).

subtract_required({Type, Required}, Allocated) ->
    %#{Type := Amt} = Allocated,
    Amt = maps:get(Type, Allocated),
    Allocated#{Type := min(0, Amt - Required)}.
