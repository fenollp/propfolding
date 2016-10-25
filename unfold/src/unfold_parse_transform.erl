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
            io:format("  ~p\n>>>~p\n\n", [S, Clause]),
            Clause1 = setelement(5, Clause, rm_match(element(5,Clause), S)),
            Clause2 = setelement(5, Clause1, replace_call(element(5,Clause1), S)),
            NewClause = Clause2,
            io:format("<<<~p\n\n", [NewClause]),
            NewClause
    end.

is_containing_rountines(Match={match,_, {var,_,Var}, RHS}
                       ,S) ->
    case is_list_of_funs(RHS, []) of
        false -> S;
        Fs ->
            S#{routines_var => Var
              ,routines => Fs
              ,match => Match
              }
    end;

is_containing_rountines(Call={call,Loc
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
    io:format("~s ::: ~p ~p ~p\n", [FName, RoutinesVar, F, Arg]),
    S#{acc0 => Acc0
      ,loc => Loc
      ,call => Call
      };

is_containing_rountines(_Expr, S) ->
    %% io:format("??? ~p\n", [_Expr]),
    S.

is_list_of_funs({cons,_, F={'fun',_, {function,_FName,1}}
                ,Cons}, Fs) ->
    is_list_of_funs(Cons, [F|Fs]);
is_list_of_funs({cons,_, F={'fun',_, {clauses,[{clause,_, [{var,_,_}],_FGuards,_FBody}]}}
                ,Cons}, Fs) ->
    is_list_of_funs(Cons, [F|Fs]);
is_list_of_funs({nil,_}, Fs) -> Fs;
is_list_of_funs(_1, _) ->
    %% io:format("fs? ~p\n", [_1]),
    false.


rm_match([], _) -> [];
rm_match([Match | Rest], #{match := Match}) ->
    Rest;
rm_match([NotAMatch | Rest], S) ->
    [NotAMatch | rm_match(Rest, S)].

replace_call([], _) -> [];
replace_call([Call | Rest]
            ,#{call := Call
              ,loc := Loc
              ,acc0 := Acc0
              ,routines := Routines
              }) ->
    NewCall = calls(Routines, Acc0, Loc),
    [NewCall|Rest];
replace_call([NotACall|Rest], S) ->
    [NotACall | replace_call(Rest, S)].

loc(Loc, Node) ->
    setelement(2, Node, Loc).

calls([], Acc0, Loc) -> loc(Loc, Acc0);
calls([F|Routines], Acc0, Loc) ->
    fun_to_call(F, Loc, calls(Routines, Acc0, Loc)).

fun_to_call(F={'fun',_FLoc,{function,_F,_A}}, Loc, Arg) ->
    {call,Loc, F, [Arg]};
fun_to_call(F={'fun',_, {clauses,[{clause,_, [{var,_,_}],_FGuards,_FBody}]}}, Loc, Arg) ->
    {call,Loc, F, [Arg]}.

%% End of Module.
