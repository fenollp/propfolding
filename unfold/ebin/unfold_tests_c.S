{module, unfold_tests_c}.  %% version = 0

{exports, [{c,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 17}.


{function, c, 0, 2}.
  {label,1}.
    {line,[{location,"test/unfold_tests_c.erl",15}]}.
    {func_info,{atom,unfold_tests_c},{atom,c},0}.
  {label,2}.
    {allocate_zero,3,0}.
    {make_fun2,{f,16},0,0,0}.
    {move,{x,0},{y,2}}.
    {make_fun2,{f,14},0,0,0}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,12},0,0,0}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,10},0,0,0}.
    {test_heap,8,1}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{y,0},{x,0},{x,0}}.
    {put_list,{y,1},{x,0},{x,0}}.
    {put_list,{y,2},{x,0},{y,2}}.
    {make_fun2,{f,8},0,0,0}.
    {move,{y,2},{x,2}}.
    {move,{literal,{r,undefined,undefined,undefined,undefined,undefined}},
          {x,1}}.
    {line,[{location,"test/unfold_tests_c.erl",21}]}.
    {call_ext_last,3,{extfunc,lists,foldl,3},3}.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,unfold_tests_c},{atom,module_info},0}.
  {label,4}.
    {move,{atom,unfold_tests_c},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,unfold_tests_c},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,unfold_tests_c},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-c/0-fun-4-', 2, 8}.
  {label,7}.
    {line,[{location,"test/unfold_tests_c.erl",21}]}.
    {func_info,{atom,unfold_tests_c},{atom,'-c/0-fun-4-'},2}.
  {label,8}.
    {allocate,0,2}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"test/unfold_tests_c.erl",21}]}.
    {call_fun,1}.
    {deallocate,0}.
    return.


{function, '-c/0-fun-3-', 1, 10}.
  {label,9}.
    {line,[{location,"test/unfold_tests_c.erl",19}]}.
    {func_info,{atom,unfold_tests_c},{atom,'-c/0-fun-3-'},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{literal,"DEUX"},{x,2}}.
    {move,{integer,3},{x,0}}.
    {line,[{location,"test/unfold_tests_c.erl",19}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, '-c/0-fun-2-', 1, 12}.
  {label,11}.
    {line,[{location,"test/unfold_tests_c.erl",18}]}.
    {func_info,{atom,unfold_tests_c},{atom,'-c/0-fun-2-'},1}.
  {label,12}.
    {move,{x,0},{x,1}}.
    {move,{literal,"cinq"},{x,2}}.
    {move,{integer,6},{x,0}}.
    {line,[{location,"test/unfold_tests_c.erl",18}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, '-c/0-fun-1-', 1, 14}.
  {label,13}.
    {line,[{location,"test/unfold_tests_c.erl",17}]}.
    {func_info,{atom,unfold_tests_c},{atom,'-c/0-fun-1-'},1}.
  {label,14}.
    {move,{x,0},{x,1}}.
    {move,{literal,"trois"},{x,2}}.
    {move,{integer,4},{x,0}}.
    {line,[{location,"test/unfold_tests_c.erl",17}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, '-c/0-fun-0-', 1, 16}.
  {label,15}.
    {line,[{location,"test/unfold_tests_c.erl",16}]}.
    {func_info,{atom,unfold_tests_c},{atom,'-c/0-fun-0-'},1}.
  {label,16}.
    {move,{x,0},{x,1}}.
    {move,{literal,"deux"},{x,2}}.
    {move,{integer,3},{x,0}}.
    {line,[{location,"test/unfold_tests_c.erl",16}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.
