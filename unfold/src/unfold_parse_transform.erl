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
    io:format(" > ~s\n", [Path]),
    [Form];
parse_transform(Form={function,_, FName, _, Clauses}) ->
    S = #{fname => FName
         ,routines => []
         },
    lists:any(fun (Clause) -> clause(S, Clause) end, Clauses)
        andalso io:format("~p\n\n", [Form]),
    [Form];
parse_transform(Form) -> [Form].

clause(S0, {clause,_, _Args, _Guards, Body}) ->
    S = lists:foldl(fun is_containing_rountines/2, S0, Body),
    R = nil =/= maps:get(routines_var, S, nil),
    R andalso io:format("\t~p\n", [S]),
    R.

is_containing_rountines({match,_, {var,_,Var}, RHS}
                       ,S) ->
    case is_list_of_funs(RHS, []) of
        false -> S;
        Fs ->
            io:format("~p --- ~p\n", [maps:get(fname, S), Var]),
            S#{routines_var => Var
              ,routines => Fs
              }
    end;
is_containing_rountines({call,Loc
                        ,{remote,_, {atom,_,lists}, {atom,_,foldl}}
                        ,[{'fun',_
                          ,{clauses,[{clause,_,
                                      [{var,_,F}, {var,_,Arg}],
                                      [],
                                      [{call,_, {var,_,F}, [{var,_,Arg}]}]}]}}
                         ,Acc0
                         ,{var,_,RoutinesVar}]}
                       ,#{fname := FName
                         ,routines_var := RoutinesVar
                         }=S) ->
    io:format("~s ::: ~p ~p ~p\n"
             ,[FName, RoutinesVar, F, Arg]),
    S#{acc0 => Acc0
      ,loc => Loc
      };
is_containing_rountines(_Expr, S) ->
    %% io:format("??? ~p\n", [_Expr]),
    S.

is_list_of_funs({cons,_, F={'fun',_, {function,_FName,1}}, Cons}, Fs) ->
    is_list_of_funs(Cons, [F|Fs]);
is_list_of_funs({nil,_}, Fs) -> Fs;
is_list_of_funs(_, _) -> false.


%% End of Module.
