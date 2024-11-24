%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_parse).

-export([parse/2]).

parse(Player, <<"n">>) ->
    log([<<"Moving n">>]),
    {Player, move, n};
parse(Player, <<"s">>) ->
    log([<<"Moving s">>]),
    {Player, move, s};
parse(Player, <<"e">>) ->
    log([<<"Moving e">>]),
    {Player, move, e};
parse(Player, <<"w">>) ->
    log([<<"Moving w">>]),
    {Player, move, w};

parse(Player, <<"get ", Item/binary>>) ->
    log([<<"Getting ">>, Item]),
    {Player, get, Item};
parse(Player, <<"drop ", Item/binary>>) ->
    log([<<"Dropping ">>, Item]),
    {Player, drop, Item};

parse(Player, <<"look">>) ->
    log([<<"Looking ">>]),
    {Player, look};
parse(Player, <<"look ", Object/binary>>) ->
    log([<<"Looking ">>, Object]),
    {Player, look, Object};

parse(Player, <<"search ", Object/binary>>) ->
    log([<<"Searching ">>, Object]),
    {Player, search, Object};

parse(Player, <<"say ", Phrase/binary>>) ->
    log([<<"Saying ">>, Phrase]),
    {Player, says, Phrase};

parse(Player, <<"shout ", Phrase/binary>>) ->
    log([<<"Shouting ">>, Phrase]),
    {Player, shouts, Phrase};

parse(Player, <<"emote ", Emote/binary>>) ->
    log([<<"Emote ">>, Emote]),
    {Player, emotes, Emote};

parse(Player, <<"cast ", SpellTarget/binary>>) ->
    log([<<"Cast ">>, SpellTarget]),

    Usage = <<"cast <spell> | cast <spell> <target>">>,
    apply_args(<<"Cast">>,
               SpellTarget,
               fun ([Spell, Target]) -> {Player, cast, Spell, on, Target};
                   ([Spell]) -> {Player, cast, Spell}
               end,
               Usage);

parse(Player, <<"attack ", Args/binary>>) ->
    Usage = <<"attack <target>">>,
    apply_args(<<"Attack">>,
               Args,
               fun ([Target]) -> {Player, attack, Target} end,
               Usage);

parse(Player, <<"buy ", Args/binary>>) ->
    Usage = <<"buy <target>">>,
    apply_args(<<"Buy">>,
               Args,
               fun ([Target]) -> {Player, buy, Target} end,
               Usage);

parse(_, Command) ->
    % TODO log to JSON
    io:format("~p:~p: Command~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Command]),
    {error, <<"Huh?">>}.

apply_args(Cmd, S, Fun, Usage) ->
    try Fun(split_args(Cmd, S, Usage))
    catch
        Error:Reason ->
            io:format("~p:~p: Failed to run command ~p with args: ~p~n~p:~p",
                      [?MODULE, ?FUNCTION_NAME, Cmd, S, Error, Reason]),
            {error, <<Usage/binary>>}
    end.

split_args(Cmd, S, Usage) ->
    Words = binary:split(string:trim(S), <<" ">>, [global]),
    case Words of
        [<<>>] ->
            io:format("~p:~p: Command ~p missing arguments", [?MODULE, ?FUNCTION_NAME, Cmd]),
            {error, <<Usage/binary>>};
        _ ->
            Words
    end.

% TODO Maybe add some loggin?
log(_Terms) ->
    ok.
