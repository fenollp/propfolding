%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(unfold_tests_d).
-compile({parse_transform, unfold_parse_transform}).

%% unfold_tests: tests for module unfold.

-export([t/0]).
-record(r, {'1', '2', '3', '4', '5'}).

%% API tests.

t() ->
    Fs = [fun two/1
         ,fun three/1
         ,fun five/1
         ,fun 'TWO'/1
         ],
    lists:foldl(fun (F, R) -> F(R) end, #r{}, Fs).

%% Internals

two(R) -> setelement(1+2, R, "deux").
'TWO'(R) -> setelement(1+2, R, "DEUX").
three(R) -> setelement(1+3, R, "trois").
five(R) -> setelement(1+5, R, "cinq").

%% End of Module.
