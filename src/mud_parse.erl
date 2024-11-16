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

parse(Player, Command = <<"cast ", SpellTarget/binary>>) ->
    log([<<"Cast ">>, SpellTarget]),
    case binary:split(string:trim(SpellTarget), <<" ">>, [global]) of
        [Spell, Target] when size(Spell) > 0, size(Target) > 0 ->
            {Player, cast, Spell, on, Target};
        [Spell] when size(Spell) > 0 ->
            {Player, cast, Spell};
        _ ->
            io:format("~p:~p: Command: ~p", [?MODULE, ?FUNCTION_NAME, Command]),
            {error, <<"What do you mean by '", Command/binary, "'">>}
    end;

parse(_, Command) ->
    % TODO log to JSON
    io:format("~p:~p: Command~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Command]),
    {error, <<"Huh?">>}.

% TODO Maybe add some loggin?
log(_Terms) ->
    ok.
