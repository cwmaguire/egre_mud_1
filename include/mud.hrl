-include_lib("egre/include/egre.hrl").

-define(PID(Value), {pid, Value}).

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

-define(RULES_MOD, {rules_module, mud_util:rules_mod_suffix(?MODULE)}).

-define(SUCCEED_SUB, #result{props = Props, log = Log}).
-define(SUCCEED_MAYBE_SUB(ShouldSubscribe), #result{subscribe = ShouldSubscribe, props = Props, log = Log}).
-define(SUCCEED_SUB_NEW_EVENT(NewEvent), #result{event = NewEvent, props = Props, log = Log}).
-define(SUCCEED_NOSUB, #result{subscribe = false, props = Props, log = Log}).
-define(FAIL_NOSUB(Reason), #result{result = {fail, Reason}, subscribe = false, props = Props, log = Log}).
-define(RESEND_SUB(Pid, NewEvent), #result{result = {resend, Pid, NewEvent}, props = Props, log = Log}).
-define(RESEND_NOSUB(Pid, NewEvent), #result{result = {resend, Pid, NewEvent}, subscribe = false, props = Props, log = Log}).
