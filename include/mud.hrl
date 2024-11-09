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
