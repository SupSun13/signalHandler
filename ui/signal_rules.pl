:- dynamic(signal/5).

signal_length(signal(_, Type, P1, P2, _), Length) :-
    (Type == digital ->
        sum_list(P1, Length)
    ; Type == analog ->
        last(P2, LastTime),
        nth0(0, P2, FirstTime),
        Length is LastTime - FirstTime
    ).

signal_max_value(signal(_, Type, P1, P2, _), Max) :-
    (Type == sinusoidal ->
        Max is P1
    ; Type == digital ->
        Max is P2
    ; Type == analog ->
        max_list(P1, Max)
    ).

longest_digital(Id) :-
    findall(Length-Id, (
        signal(Id, digital, Durations, HighLevel, _),
        signal_length(signal(Id, digital, Durations, HighLevel, _), Length)
    ), Pairs),
    keysort(Pairs, Sorted),
    reverse(Sorted, [_-Id|_]).

max_value_signal(Id) :-
    findall(Max-Id, (
        signal(Id, Type, P1, P2, _),
        signal_max_value(signal(Id, Type, P1, P2, _), Max)
    ), Pairs),
    keysort(Pairs, Sorted),
    reverse(Sorted, [_-Id|_]).