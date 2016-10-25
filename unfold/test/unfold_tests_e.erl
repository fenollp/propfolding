%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(unfold_tests_e).

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
    pipe(#r{}, Fs).

%% Internals

pipe(Acc0, Routines) ->
    lists:foldl(fun (F, R) -> F(R) end, Acc0, Routines).

two(R) -> setelement(1+2, R, "deux").
'TWO'(R) -> setelement(1+2, R, "DEUX").
three(R) -> setelement(1+3, R, "trois").
five(R) -> setelement(1+5, R, "cinq").

%% End of Module.
