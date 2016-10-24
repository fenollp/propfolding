%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(unfold_parse_transform).

%% unfold: 

-export([parse_transform/2
        ,format_error/1
        ]).


%% API

parse_transform(Forms, _Options) ->
    lists:flatmap(fun parse_transform/1, Forms).

format_error(E) ->
    case io_lib:deep_char_list(E) of
        true -> E;
        _ -> io_lib:write(E)
    end.

%% Internals

parse_transform(Form={attribute,1,file,{Path,1}}) ->
    io:format("\n>>> ~s\n", [Path]),
    [Form];
parse_transform(Form={function,_, _, _, Clauses}) ->
    io:format("~p\n", [Form]),
    io:format("--> ~p\n", [lists:map(fun clause/1, Clauses)]),
    [Form];
parse_transform(Form) -> [Form].

clause({clause,_, _Args, _Guards, Body}) ->
    lists:any(fun is_containing_rountines/1, Body).

is_containing_rountines({match,_, {var,_,_Var}, RHS}) ->
    %%store Var?
    is_list_of_funs(RHS);
is_containing_rountines({call,_
                        ,{remote,_, {atom,_,lists}, {atom,_,foldl}}
                        ,[{'fun',_
                          ,{clauses,[{clause,_,
                                      [{var,_,F}, {var,_,Arg}],
                                      [],
                                      [{call,_, {var,_,F}, [{var,_,Arg}]}]}]}}
                         ,{record,21,r,[]}
                         ,{var,_,_RoutinesVar}]}) ->
    true;
is_containing_rountines(_Expr) ->
    io:format("??? ~p\n", [_Expr]),
    false.

is_list_of_funs({cons,_, {'fun',_, {function,_FName,_Arity}}, Cons}) ->
    is_list_of_funs(Cons);
is_list_of_funs({nil,_}) -> true;
is_list_of_funs(_) -> false.


%% End of Module.
