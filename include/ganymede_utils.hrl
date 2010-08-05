to_list(Term) when is_list(Term) -> Term;
to_list(Term) when is_binary(Term) -> binary_to_list(Term).

to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) when is_list(Term) -> list_to_binary(Term).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;

hex(N) when N >= 10, N < 16 ->
    $a + (N-10).