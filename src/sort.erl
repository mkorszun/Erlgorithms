-module(sort).

-export([bubble_sort/1, bubble_sort/2]).

%% ###############################################################
%% Bubble sort
%% ###############################################################

bubble_sort(Array) ->
    bubble_sort(Array, fun comp/2).

bubble_sort(Array, Comparator) ->
    lists:foldl(fun(_, L) -> bubble_sort_it(L, Comparator) end, Array, Array).

bubble_sort_it([H1, H2 | T], Comparator) ->
    case Comparator(H1, H2) of
        1 -> [H1 | bubble_sort_it([H2 | T], Comparator)];
        0 -> [H1, H2 | bubble_sort_it(T, Comparator)];
        -1 -> [H2 | bubble_sort_it([H1 | T], Comparator)]
    end;
bubble_sort_it([H | _], _) -> [H].

%% ###############################################################
%% Internal functions
%% ###############################################################

comp(E1, E2) when E1 > E2 -> -1;
comp(E1, E2) when E1 == E2 -> 0;
comp(E1, E2) when E1 < E2 -> 1.

%% ###############################################################
%% Tests
%% ###############################################################

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

methods() -> [fun bubble_sort/1].

sort_empty_test() ->
    lists:foreach(fun(M) -> ?assertEqual([], M([])) end, methods()).

sort_sorted_test() ->
    lists:foreach(fun(M) -> ?assertEqual([1,2,3,4,5,6,7,8,9,10], M([1,2,3,4,5,6,7,8,9,10])) end, methods()).

sort_test() ->
    lists:foreach(fun(M) -> ?assertEqual([1,2,3,4,4,5,6,7,8,9,10], M([1,5,2,3,6,4,4,10,9,8,7])) end, methods()).

sort_reverse_test() ->
    lists:foreach(fun(M) -> ?assertEqual([10,9,8,7,6,5,4,3,2,1], bubble_sort([1,5,2,3,6,4,10,9,8,7], fun(E1, E2) -> comp(E1, E2) * -1 end)) end, methods()).

-endif.

%% ###############################################################
%% ###############################################################
%% ###############################################################