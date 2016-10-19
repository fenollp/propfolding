# Erlang compiler optimisation weirdnesses

## Missed optimisation with `fun Name/A`?

```erlang
-module(multiset_record).
-export([c/0, d/0]).
-include("records.hrl").

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
```

```erlang
{function, c, 0, 6}.
  {label,5}.
    {line,[{location,"src/multiset_record.erl",29}]}.
    {func_info,{atom,multiset_record},{atom,c},0}.
  {label,6}.
    {allocate_zero,3,0}.
    {make_fun2,{f,42},0,0,0}.
    {move,{x,0},{y,2}}.
    {make_fun2,{f,40},0,0,0}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,38},0,0,0}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,36},0,0,0}.
    {test_heap,8,1}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{y,0},{x,0},{x,0}}.
    {put_list,{y,1},{x,0},{x,0}}.
    {put_list,{y,2},{x,0},{y,2}}.
    {make_fun2,{f,34},0,0,0}.
    {move,{y,2},{x,2}}.
    {move,{literal,{r,undefined,undefined,undefined,undefined,undefined}},
          {x,1}}.
    {line,[{location,"src/multiset_record.erl",35}]}.
    {call_ext_last,3,{extfunc,lists,foldl,3},3}.


{function, d, 0, 8}.
  {label,7}.
    {line,[{location,"src/multiset_record.erl",37}]}.
    {func_info,{atom,multiset_record},{atom,d},0}.
  {label,8}.
    {allocate_zero,1,0}.
    {make_fun2,{f,32},0,0,0}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{y,0}}.
    {make_fun2,{f,30},0,0,0}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{y,0}}.
    {make_fun2,{f,28},0,0,0}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{y,0}}.
    {make_fun2,{f,26},0,0,0}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{y,0}}.
    {make_fun2,{f,24},0,0,0}.
    {move,{y,0},{x,2}}.
    {move,{literal,{r,undefined,undefined,undefined,undefined,undefined}},
          {x,1}}.
    {line,[{location,"src/multiset_record.erl",43}]}.
    {call_ext_last,3,{extfunc,lists,foldl,3},1}.
```

## `inline_list_funcs` has bad behaviour on non-list code

```erlang
-module(multiset_record_).
-compile([inline_list_funcs]). %% This line disables the optimisation
-export([b/0]).
-include("records.hrl").

b() ->
    R0 = #r{},
    R1 = setelement(1+2, R0, "deux"),
    R2 = setelement(1+3, R1, "trois"),
    R3 = setelement(1+5, R2, "cinq"),
    R4 = setelement(1+2, R3, "DEUX"),
    R4.
```

```erlang
%% Optimized (no inline_list_funcs):
{function, b, 0, 4}.
  {label,3}.
    {line,[{location,"src/multiset_record.erl",21}]}.
    {func_info,{atom,multiset_record},{atom,b},0}.
  {label,4}.
    {move,{literal,{r,undefined,"DEUX","trois",undefined,"cinq"}},{x,0}}.
    return.

%% Weirdly optimized:
{function, b, 0, 4}.
  {label,3}.
    {line,[{location,"src/multiset_record_.erl",22}]}.
    {func_info,{atom,multiset_record_},{atom,b},0}.
  {label,4}.
    {allocate,0,0}.
    {line,[{location,"src/multiset_record_.erl",24}]}.
    {gc_bif,'+',{f,0},0,[{integer,1},{integer,2}],{x,0}}.
    {move,{literal,"deux"},{x,2}}.
    {move,{literal,{r,undefined,undefined,undefined,undefined,undefined}},
          {x,1}}.
    {line,[{location,"src/multiset_record_.erl",24}]}.
    {call_ext,3,{extfunc,erlang,setelement,3}}.
    {line,[{location,"src/multiset_record_.erl",25}]}.
    {gc_bif,'+',{f,0},1,[{integer,1},{integer,3}],{x,3}}.
    {move,{literal,"trois"},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,3},{x,0}}.
    {line,[{location,"src/multiset_record_.erl",25}]}.
    {call_ext,3,{extfunc,erlang,setelement,3}}.
    {line,[{location,"src/multiset_record_.erl",26}]}.
    {gc_bif,'+',{f,0},1,[{integer,1},{integer,5}],{x,3}}.
    {move,{literal,"cinq"},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,3},{x,0}}.
    {line,[{location,"src/multiset_record_.erl",26}]}.
    {call_ext,3,{extfunc,erlang,setelement,3}}.
    {line,[{location,"src/multiset_record_.erl",27}]}.
    {gc_bif,'+',{f,0},1,[{integer,1},{integer,2}],{x,3}}.
    {move,{literal,"DEUX"},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,3},{x,0}}.
    {line,[{location,"src/multiset_record_.erl",27}]}.
    {call_ext_last,3,{extfunc,erlang,setelement,3},0}.
```
