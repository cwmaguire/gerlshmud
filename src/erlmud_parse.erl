-module(erlmud_parse).

-export([command/1]).

command("s") ->
    south.
