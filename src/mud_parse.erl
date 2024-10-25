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

parse(_, Command) ->
    % TODO log to JSON
    io:format("~p:~p: Command~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Command]),
    {error, <<"Huh?">>}.

% TODO Maybe add some loggin?
log(_Terms) ->
    ok.
