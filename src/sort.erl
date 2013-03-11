-module(sort).

-export([bubble_sort/1, bubble_sort/2]).
-export([insertion_sort/1, insertion_sort/2]).
-export([quick_sort/1, quick_sort/2]).

%% ###############################################################
%% Bubble sort
%% ###############################################################

bubble_sort(Array) ->
    bubble_sort(Array, fun comp/2).

bubble_sort(Array, Comparator) ->
    lists:foldl(fun(_, L) -> bubble_sort_it(L, Comparator) end, Array, Array).

%% ###############################################################

bubble_sort_it([H | []], _) -> [H];
bubble_sort_it([H1, H2 | T], Comparator) ->
    case Comparator(H1, H2) of
        1 -> [H1 | bubble_sort_it([H2 | T], Comparator)];
        0 -> [H1, H2 | bubble_sort_it(T, Comparator)];
        -1 -> [H2 | bubble_sort_it([H1 | T], Comparator)]
    end.

%% ###############################################################
%% Insertion sort
%% ###############################################################

insertion_sort(Array) ->
    insertion_sort(Array, fun comp/2).

insertion_sort(Array, Comparator) ->
    insertion_sort_it([], Array, Comparator).

%% ###############################################################

insertion_sort_it(Sorted, [], _)  ->
    Sorted;
insertion_sort_it(Sorted, [H|T], Comparator) ->
    insertion_sort_it(insert(Sorted, H, Comparator), T, Comparator).

insert([], Element, _) ->
    [Element];
insert([H|T]=A, Element, Comparator) ->
    case Comparator(H, Element) of
        1 -> [H | insert(T, Element, Comparator)];
        _ -> [Element | A]
    end.

%% ###############################################################
%% Quick sort
%% ###############################################################

quick_sort(Array) ->
    quick_sort(Array, fun comp/2).

quick_sort([], _) ->
    [];
quick_sort([P|Array], Comparator) ->
    quick_sort([X || X <- Array, Comparator(X, P) == 1], Comparator)
    ++ [P] ++
    quick_sort([X || X <- Array, Comparator(X, P) == 0 orelse Comparator(X, P) == -1], Comparator).

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

methods(1) -> [fun bubble_sort/1, fun insertion_sort/1, fun quick_sort/1];
methods(2) -> [fun bubble_sort/2, fun insertion_sort/2, fun quick_sort/2].

sort_empty_test() ->
    lists:foreach(fun(M) -> ?assertEqual([], M([])) end, methods(1)).

sort_equal_test() ->
    lists:foreach(fun(M) -> ?assertEqual([10,10,10], M([10,10,10])) end, methods(1)).

sort_sorted_test() ->
    lists:foreach(fun(M) -> ?assertEqual([1,2,3,4,5,6,7,8,9,10], M([1,2,3,4,5,6,7,8,9,10])) end, methods(1)).

sort_test() ->
    lists:foreach(fun(M) -> ?assertEqual([1,2,3,4,4,5,6,7,8,9,10], M([1,5,2,3,6,4,4,10,9,8,7])) end, methods(1)).

sort_reverse_test() ->
    lists:foreach(fun(M) -> ?assertEqual([10,9,8,7,6,5,4,3,2,1], M([1,5,2,3,6,4,10,9,8,7], fun(E1, E2) -> comp(E1, E2) * -1 end)) end, methods(2)).

-endif.

%% ###############################################################
%% ###############################################################
%% ###############################################################