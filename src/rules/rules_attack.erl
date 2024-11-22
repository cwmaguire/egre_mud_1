%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

%% If our character is attacking and we're not, tell ourself, specifically, to attempt an attack
attempt({#{character := Character},
         Props,
         {Character, Attack, Target},
         _})
  when is_pid(Target),
       Attack == attack; Attack == counter_attack ->
    Log = [{?SOURCE, Character},
           {?EVENT, Attack},
           {?TARGET, Target},
           ?RULES_MOD],
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case IsAttacking of
        false ->
            egre_object:attempt(self(),
                                {Character, attack, Target,
                                 with, self(),
                                 infinity, times});
         _ ->
             ok
    end,
    ?SUCCEED_NOSUB;

%% We've told ourself, specifically, to attack but can't, then fail the attempt
%% that is specific to us
attempt({#{character := Character},
         Props,
         {Character, Attack, Target, with, Self, _Times, times},
         _})
  when Self == self(),
       Attack == attack;
       Attack == counter_attack ->

    IsAttacking = proplists:get_value(is_attacking, Props, false),
    Log = [{?EVENT, attack},
           {?SOURCE, Character},
           {?TARGET, Target},
           {is_attacking, IsAttacking}],

    case (not IsAttacking) andalso should_attack(Props) of
        true ->
            ?SUCCEED_SUB;
        _ ->
            %% If _other_ vectors aren't yet attacking the Target then they'll join in.
            %% I'm not sure how that would happen unless the player can set what they're
            %% attacking with for each individual attack. In that case they'll need to
            %% set what their default counterattack is.
            ct:pal("~p not attacking", [self()]),
            ?SUCCEED_NOSUB
    end;

attempt({#{},
         Props,
         {Resource, allocate, Required, 'of', Type, to, Self, for, attack},
         _})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Required},
           {resource_type, Type},
           {?SOURCE, Resource},
           {?TARGET, Self}],
    ?SUCCEED_SUB;

attempt({#{},
         Props,
         {Attacker, killed, Target, with, AttackVector},
         _}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, killed},
           {?SOURCE, Target},
           {vector, AttackVector}],
    case proplists:get_value(target, Props) of
        Target_ when Target_ == Target ->
            Log2 = [{?TARGET, Target} | Log],
            #result{props = Props, log = Log2};
        _ ->
            ?SUCCEED_NOSUB
    end;

attempt({#{character := Character}, Props, {Character, stop_attack}, _}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, stop_attack}],
    ?SUCCEED_SUB;

attempt({#{character := Character},
         Props,
         {Character, die},
         _}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, die}],
    ?SUCCEED_SUB;

attempt(_) ->
    undefined.

succeed({Props, {Attacker, killed, Target, with, AttackVector}, _}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Attacker},
           {?TARGET, Target},
           {vector, AttackVector}],
    Character = proplists:get_value(character, Props),
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {?TARGET, undefined}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false}),
    {Props3, Log};

succeed({Props, {Character, Attack, Target}, _})
  when is_pid(Target),
       Attack == attack;
       Attack == counter_attack ->
    Log = [{?SOURCE, Character},
           {?EVENT, Attack},
           {?TARGET, Target},
           {rules_module, attack}],
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case IsAttacking of
        false ->
            egre:attempt(self(),
                         {Character, attack, Target,
                          with, self(),
                          infinity, times});
         _ ->
             ok
     end,
     {Props, Log};

succeed({Props, {Attacker, Attack, Target, with, Self, Times, times}, _})
  when Self == self(),
       Attack == attack; Attack == counter_attack ->
    Log = [{?EVENT, attack},
           {?SOURCE, Attacker},
           {?TARGET, Target},
           {rules_module, attack}],
    Character = proplists:get_value(character, Props),
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case IsAttacking of
        false ->
            reserve(Character, Props, Times),
            Props2 = lists:keystore(target, 1, Props, {target, Target}),
            Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, true}),
            {Props3, Log};
        _ ->
            {Props, Log}
    end;

succeed({Props, {Character, stop_attack}, _}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, stop_attack},
           {rules_module, attack}],
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false}),
    {Props3, Log};

succeed({Props, {Character, die}, _}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, die},
           {rules_module, attack}],
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false}),
    {Props3, Log};

succeed({Props, {Resource, allocate, Amt, 'of', Type, to, Self, for, attack}, _})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Amt},
           {resource_type, Type},
           {?SOURCE, Resource},
           {?TARGET, Self},
           {rules_module, attack}],
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

should_attack(Props) ->
    ShouldAttackModule = proplists:get_value(should_attack_module, Props),
    ShouldAttackModule:should_attack(Props).

reserve(Char, Props, Times) when is_list(Props) ->
    Resources = proplists:get_value(resources, Props, []),
    [reserve(Char, Res, Times, Amt) || {Res, Amt} <- Resources].

reserve(Character, Resource, Times, Amount) ->
    egre_object:attempt(self(),
                        {Character, reserve, Amount,
                         'of', Resource,
                         for, self(),
                         for, attack,
                         Times, times}).

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
