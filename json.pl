%% Utils

read_file(F, B) :-
    open(F, read, S),
    read_string(S, _, B),
    close(S).

%% Lexer

collect_digits([C|Cs], R, [C|Ds]) :-
    char_type(C, digit), !,
    collect_digits(Cs, R, Ds).
collect_digits(Cs, Cs, []).

token_number([C|Cs], R, token_number(N)) :-
    char_type(C, digit),
    collect_digits([C|Cs], R, Ds),
    number_codes(N, Ds).

collect_chars([], _, _) :- !, fail.
collect_chars([0'\" | R], R, []) :- !.
collect_chars([C | Cs], A, [C | Rs]) :-
    C \= 0'\", !,
    collect_chars(Cs, A, Rs).

token_string([], _, _) :- fail.
token_string([0'\" | Cs], R, token_string(S)) :-
    collect_chars(Cs, R, Rs),
    string_codes(S, Rs).
token_string(Input, _, _) :-
    Input = [C|_], C \= 0'\", !, fail.

tokens([], [], []).
tokens([0', | Cs], R, [token_comma() | Ts]) :- !, tokens(Cs, R, Ts).
tokens([0': | Cs], R, [token_colon() | Ts]) :- !, tokens(Cs, R, Ts).
tokens([0'{ | Cs], R, [token_open_braces() | Ts]) :- !, tokens(Cs, R, Ts).
tokens([0'} | Cs], R, [token_close_braces() | Ts]) :- !, tokens(Cs, R, Ts).
tokens([0'[ | Cs], R, [token_open_brackets() | Ts]) :- !, tokens(Cs, R, Ts).
tokens([0'] | Cs], R, [token_close_brackets() | Ts]) :- !, tokens(Cs, R, Ts).
tokens([0'n, 0'u, 0'l, 0'l | Cs], R, [token_null() | Ts]) :- !, tokens(Cs, R, Ts).
tokens([0't, 0'r, 0'u, 0'e | Cs], R, [token_true() | Ts]) :- !, tokens(Cs, R, Ts).
tokens([0'f, 0'a, 0'l, 0's, 0'e | Cs], R, [token_false() | Ts]) :- !, tokens(Cs, R, Ts).
tokens([C|Cs], R, Ts) :-
    char_type(C, space),
    !,
    tokens(Cs, R, Ts).
tokens(C, R, [T|Ts]) :-
    ( token_number(C, N, T)
    ; token_string(C, N, T) ),
    !,
    tokens(N, R, Ts).

lexer(S, T) :-
    string_codes(S, C),
    tokens(C, [], T).

%% Parser

parse_list([token_close_brackets() | R], R, []).
parse_list([token_comma() | Ts], Rr, [V | Vs]) :-
    parser(Ts, R, V),
    parse_list(R, Rr, Vs).

parse_object([token_close_braces() | R], R, _{}).
parse_object([token_comma(), token_string(S), token_colon() | Ts], Rr, O) :-
    atom_string(K, S),
    parser(Ts, R, V),
    parse_object(R, Rr, D),
    O = D.put(K, V).

parser([token_null() | R], R, 0).
parser([token_false() | R], R, 0).
parser([token_true() | R], R, 1).
parser([token_number(N) | R], R, N).
parser([token_string(S) | R], R, S).
parser([token_open_brackets(), token_close_brackets() | R], R, []).
parser([token_open_brackets() | Ts], Rr, [V | Vs]) :-
    parser(Ts, R, V),
    parse_list(R, Rr, Vs).
parser([token_open_braces(), token_close_braces() | R], R, _{}).
parser([token_open_braces(), token_string(S), token_colon() | Ts], Rr, O) :-
    atom_string(K, S),
    parser(Ts, R, V),
    parse_object(R, Rr, D),
    O = D.put(K, V).
