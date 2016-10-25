%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(unfold_tests_a).
-compile({parse_transform, unfold_parse_transform}).

%% unfold_tests: tests for module unfold.

-export([t/0]).
-record(r, {'1', '2', '3', '4', '5'}).

%% API tests.

t() ->
    #r{'2' = "DEUX"
      ,'3' = "trois"
      ,'5' = "cinq"
      }.

%% Internals

%% End of Module.
