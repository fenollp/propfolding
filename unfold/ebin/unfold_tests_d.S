{module, unfold_tests_d}.  %% version = 0

{exports, [{d,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 15}.


{function, d, 0, 2}.
  {label,1}.
    {line,[{location,"test/unfold_tests_d.erl",15}]}.
    {func_info,{atom,unfold_tests_d},{atom,d},0}.
  {label,2}.
    {allocate,0,0}.
    {move,{literal,{r,undefined,undefined,undefined,undefined,undefined}},
          {x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",21}]}.
    {call,1,{f,4}}.
    {line,[{location,"test/unfold_tests_d.erl",21}]}.
    {call,1,{f,8}}.
    {line,[{location,"test/unfold_tests_d.erl",21}]}.
    {call,1,{f,10}}.
    {call_last,1,{f,6},0}.


{function, two, 1, 4}.
  {label,3}.
    {line,[{location,"test/unfold_tests_d.erl",25}]}.
    {func_info,{atom,unfold_tests_d},{atom,two},1}.
  {label,4}.
    {move,{x,0},{x,1}}.
    {move,{literal,"deux"},{x,2}}.
    {move,{integer,3},{x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",25}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, 'TWO', 1, 6}.
  {label,5}.
    {line,[{location,"test/unfold_tests_d.erl",26}]}.
    {func_info,{atom,unfold_tests_d},{atom,'TWO'},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{literal,"DEUX"},{x,2}}.
    {move,{integer,3},{x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",26}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, three, 1, 8}.
  {label,7}.
    {line,[{location,"test/unfold_tests_d.erl",27}]}.
    {func_info,{atom,unfold_tests_d},{atom,three},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{literal,"trois"},{x,2}}.
    {move,{integer,4},{x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",27}]}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.


{function, five, 1, 10}.
  {label,9}.
    {line,[{location,"test/unfold_tests_d.erl",28}]}.
    {func_info,{atom,unfold_tests_d},{atom,five},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{literal,"cinq"},{x,2}}.
    {move,{integer,6},{x,0}}.
    {line,[{location,"test/unfold_tests_d.erl",28}]}.
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
