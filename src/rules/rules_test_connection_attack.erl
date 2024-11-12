%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_test_connection_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

is_dead_action(revive) ->
    true;
is_dead_action(_) ->
    false.

%% TODO I think this can be removed. A lot of this attack stuff looks pretty old.
%% It is part of the test connection handlers though, so I'll have to dig into it.

attempt({#parents{owner = Owner},
         Props,
         _Msg = {killed, Attack, Source, Owner},
         _}) ->
    % This is just a guess, per the message above I don't know if this is used
    Log = [{?EVENT, killed},
           {?SOURCE, Source},
           {?TARGET, Owner},
           {vector, Attack}],
    ?SUCCEED_SUB;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {Owner, die},
         _}) ->
    Log = [{?EVENT, die},
           {?SOURCE, Owner}],
    ?SUCCEED_SUB;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {Action, Attack, Owner, Target, _},
         _})
    when Action == calc_hit; Action == calc_damage ->
    Log = [{?EVENT, Action},
           {?SOURCE, Owner},
           {?TARGET, Target},
           {vector, Attack}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot ~p ~p when ~p is dead~n", [Owner, Action, Target, Owner]),
            ?FAIL_NOSUB(target_is_dead);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {Action, Attack, Attacker, Owner, _},
         _})
    when Action == calc_hit; Action == calc_damage ->
    Log = [{?EVENT, Action},
           {vector, Attack},
           {?SOURCE, Attacker},
           {?TARGET, Owner}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot ~p ~p when ~p is dead~n", [Attacker, Action, Owner, Owner]),
            ?FAIL_NOSUB(target_is_dead);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {calc_next_attack_wait, Attack, Attacker, Owner, _, _},
         _}) ->
    Log = [{?EVENT, calc_next_attack_wait},
           {vector, Attack},
           {?SOURCE, Attacker},
           {?TARGET, Owner}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot ~p to attack ~p when ~p is dead~n", [Attacker, calc_next_attack_wait, Owner, Owner]),
            ?FAIL_NOSUB(target_is_dead);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {calc_next_attack_wait, Attack, Owner, Target, _, _},
         _}) ->
    Log = [{?EVENT, calc_next_attack_wait},
           {vector, Attack},
           {?SOURCE, Owner},
           {?TARGET, Target}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot ~p to attack ~p when ~p is dead~n", [Owner, calc_next_attack_wait, Target, Target]),
            ?FAIL_NOSUB(target_is_dead);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {attack, Attack, Attacker, Owner},
         _}) ->
    Log = [{?EVENT, attack},
           {vector, Attack},
           {?SOURCE, Attacker},
           {?TARGET, Owner}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot attack ~p when ~p is dead~n", [Attacker, Owner, Owner]),
            ?FAIL_NOSUB(target_is_dead);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {attack, Attack, Owner, Target},
         _}) ->
    Log = [{?EVENT, attack},
           {vector, Attack},
           {?SOURCE, Owner},
           {?TARGET, Target}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            ?FAIL_NOSUB(attacker_is_dead);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#parents{owner = Owner},
         Props,
         Msg,
         _}) when Owner == element(2, Msg) ->
    Action = element(1, Msg),
    Log = [{?EVENT, Action}],
    IsAlive = proplists:get_value(is_alive, Props, false),
    IsDeadAction = is_dead_action(Action),
    case IsAlive orelse IsDeadAction of
        true ->
            ?SUCCEED_NOSUB;
        false ->
            AliveOrDead = case IsAlive of true -> "alive"; false -> "dead" end,
            FailMsg = iolist_to_binary(io_lib:format("~p cannot ~p when ~p~n",
                                                  [Owner, Action, AliveOrDead])),
            ?FAIL_NOSUB(FailMsg)
    end;
attempt({#parents{owner = Owner},
         Props,
         {calc_hit, Attack, Attacker, Owner, _},
         _}) ->
    Log = [{?EVENT, calc_hit},
           {vector, Attack},
           {?SOURCE, Attacker},
           {?TARGET, Owner}],
    case proplists:get_value(is_alive, Props) of
        false ->
            NewEvent = {killed, Attack, Attacker, Owner},
            ?RESEND_NOSUB(Attacker, NewEvent);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
    undefined.

%% This no longer seems to match any generated messages
%% See protocol.csv
succeed({Props, {killed, Attack, Source, Owner}, _}) ->
    Log = [{?EVENT, killed},
           {vector, Attack},
           {?SOURCE, Source},
           {?TARGET, Owner}],
    egre_object:attempt(self(), {Owner, die}),
    {Props, Log};
%% Why is test_connection_attack kicking off the cleanup?
%% _life_attack kicks it off too
succeed({Props, {Target, die}, _}) ->
    Log = [{?EVENT, die},
           {?SOURCE, Target}],
    Owner = proplists:get_value(owner, Props),
    Props2
    = case Target of
        X when X == Owner ->
            CorpseCleanupMilis = application:get_env(mud, corpse_cleanup_milis, 10 * 60 * 1000),
            egre_object:attempt_after(CorpseCleanupMilis, self(), {cleanup, Owner}),
            lists:keystore(is_alive, 1, Props, {is_alive, false});
        _ ->
            Props
    end,
    {Props2, Log};
succeed({Props, _Msg, _Context}) ->
    throw(should_never_happen),
    Props.

fail({Props, _Reason, _Message, _Context}) ->
    throw(should_never_happen),
    Props.
