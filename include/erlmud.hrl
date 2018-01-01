-define(PID(Value), {pid, Value}).

-record(parents, {owner :: pid(),
                  character :: pid(),
                  top_item :: pid(),
                  body_part :: pid()}).

-record(top_item, {item :: pid(),
                   is_active :: boolean(),
                   is_wielded :: boolean(),
                   ref :: reference()}).

-record(body_part, {body_part :: pid(),
                    type :: atom(),
                    ref :: reference()}).
