%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#parents{character = Character},
         Props,
         {Character, attack, Target}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, attack},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

%% If our character is attacking and we're not, tell ourself, specifically, to attempt an attack
attempt({#parents{character = Character},
         Props,
         {Character, Attack, Target}})
  when is_pid(Target),
       Attack == attack; Attack == counter_attack ->
    Log = [{?SOURCE, Character},
           {?EVENT, Attack},
           {?TARGET, Target}],
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case IsAttacking of
        false ->
            egre_object:attempt(self(), {Character, attack, Target, with, self()});
         _ ->
             ok
     end,
     {Props, Log};

%% We've told ourself, specifically, to attack but can't, then fail the attempt
%% that is specific to us
attempt({#parents{character = Character},
         Props,
         {Character, Attack, Target, with, Self}})
  when Self == self(),
       Attack == attack; Attack == counter_attack ->
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    ShouldAttack = should_attack(Props),

    Log = [{?EVENT, attack},
           {?SOURCE, Character},
           {?TARGET, Target},
           {is_attacking, IsAttacking}],

    case (not IsAttacking) andalso ShouldAttack of
        true ->
            {succeed, true, Props, Log};
        _ ->
            %% If _other_ vectors aren't yet attacking the Target then they'll join in.
            %% I'm not sure how that would happen unless the player can set what they're
            %% attacking with for each individual attack. In that case they'll need to
            %% set what their default counterattack is.
            {succeed, false, Props, Log}
    end;

attempt({#parents{},
         Props,
         {Resource, allocate, Required, 'of', Type, to, Self}})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Required},
           {resource_type, Type},
           {?SOURCE, Resource},
           {?TARGET, Self}],
    {succeed, true, Props, Log};

attempt({#parents{},
         Props,
         {Attacker, killed, Target, with, AttackVector}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, killed},
           {?SOURCE, Target},
           {vector, AttackVector}],
    case proplists:get_value(target, Props) of
        Target_ when Target_ == Target ->
            Log2 = [{?TARGET, Target} | Log],
            {succeed, true, Props, Log2};
        _ ->
            {succeed, false, Props, Log}
    end;

attempt({#parents{character = Character}, Props, {Character, stop_attack}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, stop_attack}],
    {succeed, true, Props, Log};

attempt({#parents{character = Character},
         Props,
         {Character, die}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, die}],
    {succeed, true, Props, Log};

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Attacker, killed, Target, with, AttackVector}}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Attacker},
           {?TARGET, Target},
           {vector, AttackVector}],
    Character = proplists:get_value(character, Props),
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {?TARGET, undefined}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false}),
    {Props3, Log};

succeed({Props, {Character, Attack, Target}})
  when is_pid(Target),
       Attack == attack; Attack == counter_attack ->
    Log = [{?SOURCE, Character},
           {?EVENT, Attack},
           {?TARGET, Target}],
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case IsAttacking of
        false ->
            egre_object:attempt(self(), {Character, attack, Target, with, self()});
         _ ->
             ok
     end,
     {Props, Log};

succeed({Props, {Attacker, Attack, Target, with, Self}})
  when Self == self(),
       Attack == attack; Attack == counter_attack ->
    Log = [{?EVENT, attack},
           {?SOURCE, Attacker},
           {?TARGET, Target}],
    Character = proplists:get_value(character, Props),
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case IsAttacking of
        false ->
            reserve(Character, Props),
            Props2 = lists:keystore(target, 1, Props, {target, Target}),
            Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, true}),
            {Props3, Log};
        _ ->
            {Props, Log}
    end;

succeed({Props, {Character, stop_attack}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, stop_attack}],
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false}),
    {Props3, Log};

succeed({Props, {Character, die}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, die}],
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false}),
    {Props3, Log};

succeed({Props, {Resource, allocate, Amt, 'of', Type, to, Self}})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Amt},
           {resource_type, Type},
           {?SOURCE, Resource},
           {?TARGET, Self}],
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

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

should_attack(Props) ->
    ShouldAttackModule = proplists:get_value(should_attack_module, Props),
    ShouldAttackModule:should_attack(Props).

reserve(Character, Props) when is_list(Props) ->
    [reserve(Character, Resource, Amount) || {Resource, Amount} <- proplists:get_value(resources, Props, [])].

reserve(Character, Resource, Amount) ->
    egre_object:attempt(self(),
                        {Character, reserve, Amount,
                         'of', Resource, for, self()}).

unreserve(Character, Props) when is_list(Props) ->
    [unreserve(Character, Resource) || {Resource, _Amt} <- proplists:get_value(resources, Props, [])];
unreserve(Character, Resource) ->
    egre_object:attempt(self(), {Character, unreserve, Resource, for, self()}).

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

%log(Props) ->
    %egre_event_log:log(debug, [{module, ?MODULE} | Props]).
