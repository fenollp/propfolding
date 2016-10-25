%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(unfold_tests_c).
-compile({parse_transform, unfold_parse_transform}).

%% unfold_tests: tests for module unfold.

-export([t/0]).
-record(r, {'1', '2', '3', '4', '5'}).

%% API tests.

t() ->
    Fs = [fun (R) -> setelement(1+2, R, "deux") end
         ,fun (R) -> setelement(1+3, R, "trois") end
         ,fun (R) -> setelement(1+5, R, "cinq") end
         ,fun (R) -> setelement(1+2, R, "DEUX") end
         ],
    lists:foldl(fun (F, R) -> F(R) end, #r{}, Fs).

%% Internals

%% End of Module.
