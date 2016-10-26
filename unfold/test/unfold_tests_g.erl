%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(unfold_tests_g).

%% unfold_tests: tests for module unfold.

-export([t/0]).
-record(r, {'1', '2', '3', '4', '5'}).

%% API tests.

t() ->
    R0 = #r{},
    R1 = (fun () -> setelement(1+2, R0, "deux") end)(),
    R2 = (fun () -> setelement(1+3, R1, "trois") end)(),
    R3 = (fun () -> setelement(1+5, R2, "cinq") end)(),
    R4 = (fun () -> setelement(1+2, R3, "DEUX") end)(),
    R4.

%% Internals

%% End of Module.
