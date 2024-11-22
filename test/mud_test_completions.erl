%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_test_completions).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->

    Filename = "egre_mud_1_bash_completions",
    {ok, CompletionsFile} = file:open(Filename, [write]),
    [form(CompletionsFile, Form) || Form <- Forms],
    file:close(CompletionsFile),

    Forms.

form(File,
     {function, _Line, all, 0, [{clause, _Line2, _Head, _GG, [Body]}]}) ->
    body(File, Body);
form(_Form, _File) ->
    ok.

body(_File, {nil, _Line1}) ->
    ok;
body(File, {cons, _Line1, {atom, _Line2, Function}, Cons}) ->
    io:format(File, "~p~n", [Function]),
    body(File, Cons).
