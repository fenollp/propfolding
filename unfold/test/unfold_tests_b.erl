%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(unfold_tests_b).
-compile({parse_transform, unfold_parse_transform}).

%% unfold_tests: tests for module unfold.

-export([t/0]).
-record(r, {'1', '2', '3', '4', '5'}).

%% API tests.

t() ->
    R0 = #r{},
    R1 = setelement(1+2, R0, "deux"),
    R2 = setelement(1+3, R1, "trois"),
    R3 = setelement(1+5, R2, "cinq"),
    R4 = setelement(1+2, R3, "DEUX"),
    R4.

%% Internals

%% End of Module.
