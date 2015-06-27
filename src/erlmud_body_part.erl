%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(erlmud_body_part).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

is_match(Props, Owner, Name) ->
    Owner == proplists:get_value(owner, Props) andalso
        match == re:run(proplists:get_value(name, Props, ""), Name, [{capture, none}]).

can(add, Props, ItemProps) ->
    can_add(Props, ItemProps);
can(remove, Props, ItemProps) ->
    can_remove(Props, ItemProps).

%TODO Add logic to determine if the item can be added to this body
%     part given the item properties (e.g. weight, bulk, size, grip, colour, whatever)
can_add(_Props, _ItemProps) ->
    true.

can_remove(_Props, _ItemProps) ->
    true.

%TODO I could keep sending messages back and forth for each constraint that the item
%     (or any object) could yay/nay on. Or, I could send them all at once and all
%     objects could vote en masse or even remove/add constraints as the messages fly by.
attempt(Props, {Action, Owner, {Item, _} = ItemAndProps, [_ | _] = BodyPartName})
  when is_pid(Item),
       Action == add;
       Action == remove ->
    case is_match(Props, Owner, BodyPartName) of
        true ->
            log("resending {~p, ~p, ~p, ~p} as {~p, ~p, ~p}~n",
                [Action, Owner, ItemAndProps, BodyPartName, Action, ItemAndProps, self()]),
            {{resend, Owner, {Action, ItemAndProps, self()}}, true, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt(Props, {Action, {_, ItemProps}, Self})
  when Self == self(),
       Action == add;
       Action == remove ->
    case can(Action, Props, ItemProps) of
        {false, Reason} ->
            {{fail, Reason}, false, Props};
        _ ->
            {succeed, true, Props}
    end;
attempt(Props, _Msg) ->
    {succeed, false, Props}.

succeed(Props, {add, {Item, _}, Self}) when Self == self(), is_pid(Item) ->
    log("added ~p~n", [Item]),
    Owner = proplists:get_value(owner, Props),
    erlmud_object:remove(Owner, item, Item),
    [{item, Item} | Props];
succeed(Props, {remove, {Item, _}, Self}) when Self == self(), is_pid(Item) ->
    log("added ~p~n", [Item]),
    Owner = proplists:get_value(ownder, Props),
    erlmud_object:add(Owner, item, Item),
    lists:keydelete(Item, 2, Props);
succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    Props.

fail(Props, _Message, _Reason) ->
    Props.

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
