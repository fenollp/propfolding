{module, unfold_tests_d}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{t,0}]}.

{attributes, []}.

{labels, 25}.


{function, t, 0, 2}.
  {label,1}.
    {line,[{location,"test/unfold_tests_d.erl",13}]}.
    {func_info,{atom,unfold_tests_d},{atom,t},0}.
  {label,2}.
    {allocate_zero,1,0}.
    {make_fun2,{f,24},0,0,0}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{y,0}}.
    {make_fun2,{f,22},0,0,0}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{y,0}}.
    {make_fun2,{f,20},0,0,0}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{y,0}}.
    {make_fun2,{f,18},0,0,0}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{y,0}}.
    {make_fun2,{f,16},0,0,0}.
    {move,{y,0},{x,2}}.
    {move,{literal,{r,undefined,undefined,undefined,undefined,undefined}},
          {x,1}}.
    {line,[{location,"test/unfold_tests_d.erl",19}]}.
    {call_ext_last,3,{extfunc,lists,foldl,3},1}.


{function, two, 1, 4}.
  {label,3}.
    {line,[{location,"test/unfold_tests_d.erl",23}]}.
    {func_info,{atom,unfold_tests_d},{atom,two},1}.
  {label,4}.
    {move,{x,0},{x,1}}.
    {move,{literal,"deux"},{x,2}}.
    {move,{integer,3},{x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",23}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, 'TWO', 1, 6}.
  {label,5}.
    {line,[{location,"test/unfold_tests_d.erl",24}]}.
    {func_info,{atom,unfold_tests_d},{atom,'TWO'},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{literal,"DEUX"},{x,2}}.
    {move,{integer,3},{x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",24}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, three, 1, 8}.
  {label,7}.
    {line,[{location,"test/unfold_tests_d.erl",25}]}.
    {func_info,{atom,unfold_tests_d},{atom,three},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{literal,"trois"},{x,2}}.
    {move,{integer,4},{x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",25}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, five, 1, 10}.
  {label,9}.
    {line,[{location,"test/unfold_tests_d.erl",26}]}.
    {func_info,{atom,unfold_tests_d},{atom,five},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{literal,"cinq"},{x,2}}.
    {move,{integer,6},{x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",26}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, module_info, 0, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,unfold_tests_d},{atom,module_info},0}.
  {label,12}.
    {move,{atom,unfold_tests_d},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 14}.
  {label,13}.
    {line,[]}.
    {func_info,{atom,unfold_tests_d},{atom,module_info},1}.
  {label,14}.
    {move,{x,0},{x,1}}.
    {move,{atom,unfold_tests_d},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-t/0-fun-4-', 2, 16}.
  {label,15}.
    {line,[{location,"test/unfold_tests_d.erl",19}]}.
    {func_info,{atom,unfold_tests_d},{atom,'-t/0-fun-4-'},2}.
  {label,16}.
    {allocate,0,2}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",19}]}.
    {call_fun,1}.
    {deallocate,0}.
    return.


{function, '-t/0-fun-0-', 1, 18}.
  {label,17}.
    {line,[{location,"test/unfold_tests_d.erl",14}]}.
    {func_info,{atom,unfold_tests_d},{atom,'-t/0-fun-0-'},1}.
  {label,18}.
    {call_only,1,{f,4}}.


{function, '-t/0-fun-1-', 1, 20}.
  {label,19}.
    {line,[{location,"test/unfold_tests_d.erl",15}]}.
    {func_info,{atom,unfold_tests_d},{atom,'-t/0-fun-1-'},1}.
  {label,20}.
    {call_only,1,{f,8}}.


{function, '-t/0-fun-2-', 1, 22}.
  {label,21}.
    {line,[{location,"test/unfold_tests_d.erl",16}]}.
    {func_info,{atom,unfold_tests_d},{atom,'-t/0-fun-2-'},1}.
  {label,22}.
    {call_only,1,{f,10}}.


{function, '-t/0-fun-3-', 1, 24}.
  {label,23}.
    {line,[{location,"test/unfold_tests_d.erl",17}]}.
    {func_info,{atom,unfold_tests_d},{atom,'-t/0-fun-3-'},1}.
  {label,24}.
    {call_only,1,{f,6}}.
