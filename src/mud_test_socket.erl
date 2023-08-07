%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_test_socket).
-behaviour(gen_server).

-export([start/0]).
-export([stop/0]).
-export([send/1]).
-export([messages/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {conn,
                messages = [] :: [string()]}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

send(Msg) ->
    gen_server:cast(?MODULE, Msg).

messages() ->
    gen_server:call(?MODULE, messages).

init(_) ->
  {ok, Conn} = supervisor:start_child(gerlshmud_conn_sup, [self()]),
  {ok, #state{conn = Conn}}.

handle_call(messages, _From, State = #state{messages = Messages}) ->
    {reply, Messages, State#state{messages = []}};
handle_call(_Req = Text, _From, State = #state{conn = Conn}) ->
    gerlshmud_conn:handle(Conn, Text),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Req = Text, State = #state{conn = Conn}) ->
    gerlshmud_conn:handle(Conn, Text),
    {noreply, State}.

handle_info({send, Msg}, State = #state{messages = Messages}) ->
    ct:pal("Test socket received: ~p~n", [flatten(Msg)]),
    {noreply, State#state{messages = [flatten(Msg) | Messages]}};
handle_info(_Req, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, _, _) ->
    {error, not_implemented}.

flatten(Output) when is_list(Output) ->
    ListOfBins = lists:flatten(Output),
    lists:foldl(fun(Bin, Acc) -> <<Acc/binary, Bin/binary>> end, <<>>, ListOfBins);
flatten(Output) ->
    Output.
