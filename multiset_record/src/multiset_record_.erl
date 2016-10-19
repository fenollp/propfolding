%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(multiset_record_).
-compile([inline_list_funcs]).

%% multiset_record_: see if compiler merges mutiple calls.

-export([a/0, b/0, c/0, d/0]).
-export([main/1]).

-include("records.hrl").

%% API

a() ->
    #r{'2' = "DEUX"
      ,'3' = "trois"
      ,'5' = "cinq"
      }.

b() ->
    R0 = #r{},
    R1 = setelement(1+2, R0, "deux"),
    R2 = setelement(1+3, R1, "trois"),
    R3 = setelement(1+5, R2, "cinq"),
    R4 = setelement(1+2, R3, "DEUX"),
    R4.

c() ->
    Fs = [fun (R) -> setelement(1+2, R, "deux") end
         ,fun (R) -> setelement(1+3, R, "trois") end
         ,fun (R) -> setelement(1+5, R, "cinq") end
         ,fun (R) -> setelement(1+2, R, "DEUX") end
         ],
    lists:foldl(fun (F, R) -> F(R) end, #r{}, Fs).

d() ->
    Fs = [fun two/1
         ,fun three/1
         ,fun five/1
         ,fun 'TWO'/1
         ],
    lists:foldl(fun (F, R) -> F(R) end, #r{}, Fs).

%% Internals

main(_) ->
    io:format("~p ~p\n", [fun ?MODULE:a/0, timer:tc(fun ?MODULE:a/0)]),
    io:format("~p ~p\n", [fun ?MODULE:b/0, timer:tc(fun ?MODULE:b/0)]),
    io:format("~p ~p\n", [fun ?MODULE:c/0, timer:tc(fun ?MODULE:c/0)]).

two(R) -> setelement(1+2, R, "deux").
'TWO'(R) -> setelement(1+2, R, "DEUX").
three(R) -> setelement(1+3, R, "trois").
five(R) -> setelement(1+5, R, "cinq").

%% End of Module.
