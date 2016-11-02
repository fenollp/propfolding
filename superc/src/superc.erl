%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(superc).

%% superc: 

-export([parse_transform/2]).


%% API

parse_transform(Forms, _Options) ->
    %% parse_trans:plain_transform(fun pt/1, Forms).
    case parse_trans:depth_first(fun pt/4, #{}, Forms, _Options) of
        {error, Es} -> Es ++ Forms;
        {NewForms, _} ->
            parse_trans:revert(NewForms)
    end.

%% Internals

pt (Ty, Form, Ctx, Acc) ->
    io:format(">>> Ty:~p\n\tForm:~p\n\tCtx:~p\n\tAcc:~p\n", [Ty, Form, Ctx, Acc]),
    {Form, Acc}.

pt(_Node) ->
    io:format(">>> ~p\n", [_Node]),
    continue.

%% End of Module.
