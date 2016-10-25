{module, unfold_tests_b}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{t,0}]}.

{attributes, []}.

{labels, 7}.


{function, t, 0, 2}.
  {label,1}.
    {line,[{location,"test/unfold_tests_b.erl",14}]}.
    {func_info,{atom,unfold_tests_b},{atom,t},0}.
  {label,2}.
    {move,{literal,{r,undefined,"DEUX","trois",undefined,"cinq"}},{x,0}}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,unfold_tests_b},{atom,module_info},0}.
  {label,4}.
    {move,{atom,unfold_tests_b},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,unfold_tests_b},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,unfold_tests_b},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
