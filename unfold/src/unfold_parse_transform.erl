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
parse_transform(Form={function,_, FName, Arity, Clauses}) ->
    S = #{fname => FName
         ,farity => Arity
         ,routines => []
         },
    NewClauses = lists:map(fun (Clause) -> clause(S, Clause) end, Clauses),
    [setelement(5, Form, NewClauses)];
parse_transform(Form) ->
    [Form].

clause(S0, Clause={clause,_, _Args, _Guards, Body}) ->
    S = lists:foldl(fun is_containing_rountines/2, S0, Body),
    case maps:get(routines_var, S, nil) of
        nil -> Clause;
        _ ->
            io:format("  ~p\n~p\n\n", [S, Clause]),
            Clause
    end.

is_containing_rountines({match,_, {var,_,Var}, RHS}
                       ,S) ->
    case is_list_of_funs(RHS, []) of
        false -> S;
        Fs ->
            io:format("fun ~p/~p --- ~p\n", [maps:get(fname,S), maps:get(farity,S), Var]),
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
