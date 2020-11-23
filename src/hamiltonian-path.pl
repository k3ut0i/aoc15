%% -*- mode : prolog -*-
read_all_lines_(Stream, Lines, Acc) :-
    read_line_to_string(Stream, Line),
    (Line = end_of_file -> reverse(Lines, Acc);
     read_all_lines_(Stream, Lines, [Line | Acc])).

read_all_lines(File, Lines) :-
    open(File, read, Stream),
    read_all_lines_(Stream, Lines, []),
    close(Stream).

parse_distance(String, d(A, B, Dis)) :-
    re_split("\\s+", String, [AStr, " ", "to", " ", BStr, " ", "=", " ", DStr]),
    string_lower(AStr, A1), string_lower(BStr, B1),
    atom_string(A, A1), atom_string(B, B1),
    number_string(Dis, DStr).

distances(File, Ds) :-
    read_all_lines(File, Lines),
    maplist(parse_distance, Lines, Ds).

all_places(Ds, Ps) :-
    findall([A,B], member(d(A, B, _), Ds), ABs),
    flatten(ABs, Psd), list_to_set(Psd, Ps).

total_distance([_], _, 0).
total_distance([A, B | Ps], Ds, X) :-
    (member(d(A, B, AB), Ds) ; member(d(B, A, AB), Ds)),
    total_distance([B|Ps], Ds, Rest),
    plus(AB, Rest, X).

trail(Ds, T, Dis) :-
    all_places(Ds, Ps),
    permutation(Ps, T),
    total_distance(T, Ds, Dis).
