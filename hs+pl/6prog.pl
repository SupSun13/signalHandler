:- initialization(main). 

cities([a, b, c, d]).

distance(a, b, 2). distance(a, c, 5). distance(a, d, 3).
distance(b, a, 2). distance(b, c, 4). distance(b, d, 6).
distance(c, a, 5). distance(c, b, 4). distance(c, d, 1).
distance(d, a, 3). distance(d, b, 6). distance(d, c, 1).

main :-
    cities([First|Rest]),
    find_routes(Rest, First, Routes),
    my_keysort(Routes, [MinLength-MinRoute|_]),
    format('Shortest route: ~w~nLength: ~w~n', [[First|MinRoute], MinLength]).

find_routes(Rest, First, Routes) :-
    init_permutations(Rest, Perms),
    calculate_lengths(Perms, First, Routes).

init_permutations(List, Perms) :-
    my_permutation(List, [], Perms).

my_permutation([], Acc, [Acc]).
my_permutation(List, Acc, Perms) :-
    my_select(X, List, NewList),
    my_permutation(NewList, [X|Acc], Perms).

my_select(X, [X|T], T).
my_select(X, [H|T], [H|Rest]) :- my_select(X, T, Rest).

calculate_lengths([], _, []).
calculate_lengths([Perm|Perms], First, [Length-Perm|Rest]) :-
    close_route([First|Perm], First, ClosedRoute),
    path_length(ClosedRoute, Length),
    calculate_lengths(Perms, First, Rest).

close_route(Route, First, ClosedRoute) :-
    my_reverse(Route, RevRoute),
    ClosedRoute = [First|RevRoute].

my_reverse(L, R) :- my_reverse_acc(L, [], R).
my_reverse_acc([], Acc, Acc).
my_reverse_acc([H|T], Acc, R) :- my_reverse_acc(T, [H|Acc], R).

path_length([_], 0).
path_length([A,B|T], Length) :-
    distance(A, B, D),
    path_length([B|T], L),
    Length is D + L.

my_keysort(List, Sorted) :-
    my_insertion_sort(List, Sorted).

my_insertion_sort(List, Sorted) :-
    my_foldl(my_insert, List, [], Sorted).

my_foldl(_, [], Acc, Acc).
my_foldl(Pred, [H|T], Acc, Result) :-
    call(Pred, H, Acc, NewAcc),
    my_foldl(Pred, T, NewAcc, Result).

my_insert(L-P, [], [L-P]).
my_insert(L-P, [L1-P1|T], [L-P, L1-P1|T]) :- L =< L1.
my_insert(L-P, [H|T], [H|Rest]) :- my_insert(L-P, T, Rest).