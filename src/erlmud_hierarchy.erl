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

insert({Parent, Children}, Obj) ->
    insert({Parent, Children, _PostChildren = []}, Obj);
insert({Parent, PreChildren, PostChildren}, {Parent, Obj}) ->
    {ok, {Parent, [Obj | PreChildren] ++ PostChildren}};
insert({_, _PreChildren = [], _}, _) ->
    undefined;
insert({Parent, [Child | PreChildren], PostChildren}, Obj = {_NotParent, _ObjChild}) ->
    case insert(Child, Obj) of
        {ok, NewChild} ->
            {ok, {Parent, [NewChild | PreChildren] ++ PostChildren}};
        _ ->
            insert({Parent, PreChildren, [Child | PostChildren]}, Obj)
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

