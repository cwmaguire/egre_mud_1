%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_conn).
-behaviour(gen_statem).

-export([start_link/1]).
-export([handle/2]).

-export([login/3]).
-export([password/3]).
-export([live/3]).
-export([dead/3]).

-export([init/1]).
-export([callback_mode/0]).

-record(data, {socket :: pid(),
               conn_obj :: pid(),
               player :: pid(),
               login :: string(),
               attempts = 0 :: integer()}).

%% api

start_link(Socket) ->
    gen_statem:start_link(?MODULE, Socket, []).

handle(Pid, Msg) ->
    gen_statem:cast(Pid, Msg).

%% states

login(cast, Event, Data) ->
    {next_state, password, Data#data{login = Event}}.

password(cast, _Event = Password, Data = #data{login = Login,
                                               attempts = Attempts,
                                               socket = Socket}) ->
    case is_valid_creds(Login, Password) of
        {true, logger} ->
            gerlshmud_event_log:register(self());
        {true, _Player} ->
            % All players are live processes at MUD startup; processes are almost free
            PlayerPid = gerlshmud_index:get_pid(player),
            ConnProps = [{owner, PlayerPid},
                         {conn, {self()}},
                         {handlers, [gerlshmud_handler_conn_enter_world,
                                     gerlshmud_handler_conn_send]}],

            Socket ! {send, <<"Login successful.">>},
            {ok, ConnObjPid} = supervisor:start_child(gerlshmud_object_sup, [_Id = undefined, ConnProps]),
            % TODO Player is supposed to enter in a room
            Message = {PlayerPid, enter_world, in, room, with, ConnObjPid},
            ConnObjPid ! {ConnObjPid, Message},

            {next_state, live, Data#data{login = undefined,
                                         player = PlayerPid,
                                         conn_obj = ConnObjPid}};
        false ->
            get_failed_auth_state(Data#data{login = undefined, attempts = Attempts + 1})
    end.

get_failed_auth_state(Data = #data{attempts = Attempts}) when Attempts < 3 ->
    {next_state, login, Data};
get_failed_auth_state(Data) ->
    {next_state, dead, Data}.

dead(cast, _, _Data = #data{socket = Socket}) ->
    Socket ! {send, "Connection Refused"},
    keep_state_and_data;

dead({call, From}, props, _Data) ->
    {keep_state_and_data, [{reply, From, _Props = []}]}.

live(cast, {send, Message}, _Data = #data{socket = Socket}) ->
    Socket ! {send, Message},
    keep_state_and_data;
live(cast, Event, Data = #data{player = PlayerPid, conn_obj = ConnObjPid}) ->
    log([{event, Event}, {state, live}, {player, Data}, {conn, ConnObjPid}]),

    _ = case gerlshmud_parse:parse(PlayerPid, Event) of
        {error, Error} ->
            Data#data.socket ! {send, Error};
        Message ->
            ConnObjPid ! {ConnObjPid, Message}
    end,
    {next_state, live, Data};
live({call, {From, Ref}}, props, _Data) ->
    From ! {Ref, _Props = []},
    keep_state_and_data;
live(Type, Event, Data) ->
    console_log_unknown(live, Type, Event, Data),
    keep_state_and_data.

%% gen_statem

init(Socket) ->
    Socket ! {send, <<"Welcome to GErlSHmud!">>},
    Socket ! {send, <<"Currently any login and password will do.">>},
    {ok, login, #data{socket = Socket}}.

callback_mode() ->
    state_functions.

%% private

is_valid_creds("log", "log") ->
    {true, logger};
is_valid_creds(_String, never_fails) ->
    false;
is_valid_creds(_Login, _Password) ->
    {true, gerlshmud_index:get_pid(player)}.

log(Terms) ->
    gerlshmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).

console_log_unknown(State, EventType, EventData, _Data = #data{player = Player}) ->
    io:format("Connection ~p for player ~p received unrecognized event ~p:~p in state ~p",
              [self(), Player, EventType, EventData, State]).
