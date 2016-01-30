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
-module(erlmud_hierarchy).

-export([new/1]).
-export([insert/2]).
-export([is_descendent/2]).
%-export([is_descendent/3]).

new(Parent) ->
    {Parent, []}.

insert({Parent, Children}, NewParentChild) ->
    insert({Parent, Children, _PostChildren = []}, NewParentChild);
insert({Parent, PreChildren, PostChildren}, {Parent, NewChild}) ->
    {ok, {Parent, [{NewChild, []} | PreChildren] ++ PostChildren}};
insert({_, _PreChildren = [], _}, _) ->
    undefined;
insert({Parent, [Child0 | PreChildren], PostChildren},
       NewParentChild = {_NotParent, NewChild}) ->
    ct:pal("Inserting child ~p into ~p~n", [NewChild, Child0]),
    case insert(Child0, NewParentChild) of
        {ok, Child1} ->
            ct:pal("Inserted child ~p into ~p to get ~p~n", [NewChild, Child0, Child1]),
            {ok, {Parent, [Child1 | PreChildren] ++ PostChildren}};
        _ ->
            ct:pal("Did not insert child ~p into ~p~n", [NewChild, Child0]),
            ct:pal("Inserting ~p into head of ~p~n", [NewParentChild, PreChildren]),
            insert({Parent, PreChildren, [Child0 | PostChildren]}, NewParentChild)
    end.

is_descendent(Hierarchy = {Parent, _}, Child) ->
    is_descendent(Hierarchy, Parent, Child).

is_descendent({Parent, [Child | _]}, Parent, Child) ->
    true;
is_descendent({_, []}, _, _) ->
    false;
is_descendent({_NotParent, [NotChild | Children]}, Parent, Child) ->
    is_descendent(NotChild, Parent, Child) orelse
    is_descendent({Parent, Children}, Parent, Child).

