{module, unfold_tests_g}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{t,0}]}.

{attributes, []}.

{labels, 15}.


{function, t, 0, 2}.
  {label,1}.
    {line,[{location,"test/unfold_tests_g.erl",13}]}.
    {func_info,{atom,unfold_tests_g},{atom,t},0}.
  {label,2}.
    {allocate,0,0}.
    {make_fun2,{f,14},0,0,0}.
    {line,[{location,"test/unfold_tests_g.erl",15}]}.
    {call_fun,0}.
    {make_fun2,{f,12},0,0,1}.
    {line,[{location,"test/unfold_tests_g.erl",16}]}.
    {call_fun,0}.
    {make_fun2,{f,10},0,0,1}.
    {line,[{location,"test/unfold_tests_g.erl",17}]}.
    {call_fun,0}.
    {make_fun2,{f,8},0,0,1}.
    {line,[{location,"test/unfold_tests_g.erl",18}]}.
    {call_fun,0}.
    {deallocate,0}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,unfold_tests_g},{atom,module_info},0}.
  {label,4}.
    {move,{atom,unfold_tests_g},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,unfold_tests_g},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,unfold_tests_g},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-t/0-fun-3-', 1, 8}.
  {label,7}.
    {line,[{location,"test/unfold_tests_g.erl",18}]}.
    {func_info,{atom,unfold_tests_g},{atom,'-t/0-fun-3-'},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{literal,"DEUX"},{x,2}}.
    {move,{integer,3},{x,0}}.
    {line,[{location,"test/unfold_tests_g.erl",18}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, '-t/0-fun-2-', 1, 10}.
  {label,9}.
    {line,[{location,"test/unfold_tests_g.erl",17}]}.
    {func_info,{atom,unfold_tests_g},{atom,'-t/0-fun-2-'},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{literal,"cinq"},{x,2}}.
    {move,{integer,6},{x,0}}.
    {line,[{location,"test/unfold_tests_g.erl",17}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, '-t/0-fun-1-', 1, 12}.
  {label,11}.
    {line,[{location,"test/unfold_tests_g.erl",16}]}.
    {func_info,{atom,unfold_tests_g},{atom,'-t/0-fun-1-'},1}.
  {label,12}.
    {move,{x,0},{x,1}}.
    {move,{literal,"trois"},{x,2}}.
    {move,{integer,4},{x,0}}.
    {line,[{location,"test/unfold_tests_g.erl",16}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, '-t/0-fun-0-', 0, 14}.
  {label,13}.
    {line,[{location,"test/unfold_tests_g.erl",15}]}.
    {func_info,{atom,unfold_tests_g},{atom,'-t/0-fun-0-'},0}.
  {label,14}.
    {move,{literal,{r,undefined,"deux",undefined,undefined,undefined}},{x,0}}.
    return.
