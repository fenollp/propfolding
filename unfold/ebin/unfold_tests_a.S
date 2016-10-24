{module, unfold_tests_a}.  %% version = 0

{exports, [{a,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, a, 0, 2}.
  {label,1}.
    {line,[{location,"test/unfold_tests_a.erl",15}]}.
    {func_info,{atom,unfold_tests_a},{atom,a},0}.
  {label,2}.
    {move,{literal,{r,undefined,"DEUX","trois",undefined,"cinq"}},{x,0}}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,unfold_tests_a},{atom,module_info},0}.
  {label,4}.
    {move,{atom,unfold_tests_a},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,unfold_tests_a},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,unfold_tests_a},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
