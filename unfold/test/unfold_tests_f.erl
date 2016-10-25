%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(unfold_tests_f).

%% unfold_tests: tests for module unfold.

-export([t/0]).
-record(r, {'1', '2', '3', '4', '5'}).

%% API tests.

t() ->
    S = #{some => "state"},
    Fs = [fun two/2
         ,fun three/2
         ,fun five/2
         ,fun 'TWO'/2
         ],
    lists:foldl(fun (F, R) -> F(R, S) end, #r{}, Fs).

%% Internals

two(R, _) -> setelement(1+2, R, "deux").
'TWO'(R, _) -> setelement(1+2, R, "DEUX").
three(R, _) -> setelement(1+3, R, "trois").
five(R, _) -> setelement(1+5, R, "cinq").

%% End of Module.
