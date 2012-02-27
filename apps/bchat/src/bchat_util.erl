-module(bchat_util).
-export([parse_uuid/1, format_uuid/1, random_uuid/0]).

%% UUID tools
%% Thanks https://github.com/brendonh

parse_uuid(S) when is_list(S) andalso length(S) == 36 ->
    I = erlang:list_to_integer([C || C <- S, C /= $-], 16),
    <<I:16/unsigned-integer-unit:8>>.

format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
                                [TL, TM, THV, CSR, CSL, N])).

random_uuid() ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    <<(random:uniform(4294967296) - 1):32,
     (random:uniform(4294967296) - 1):32,
     (random:uniform(4294967296) - 1):32,
     (random:uniform(4294967296) - 1):32>>.
