-define(PID(Value), {pid, Value}).

-type proplist() :: [{atom(), any()}].
-type source() :: any().
-type type() :: any().
-type target() :: any().
-type context() :: any().
-type vector() :: any().

-record(parents,
        {owner :: pid(),
         character :: pid(),
         top_item :: pid(),
         body_part :: pid()}).

-record(top_item,
        {item :: pid(),
         is_active :: boolean(),
         is_wielded :: boolean(),
         ref :: reference()}).

-record(body_part,
        {body_part :: pid(),
         type :: atom(),
         ref :: reference()}).

-record(object,
        {id :: string(),
         pid :: pid(),
         icon :: atom(),
         properties :: list()}).

-record(dead_pid_subscription,
        {subscriber :: pid(),
         dead_pid :: pid()}).

-record(replacement_pid,
        {old_pid :: pid(),
         new_pid :: pid()}).


-define(EVENT, event_type).
-define(SOURCE, event_source).
-define(TARGET, event_target).
