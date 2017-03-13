# superc

An Erlang compiler pass for [supercompilation](https://en.wikipedia.org/wiki/Metacompilation) (stream fusion, partial evaluation and other compile-time optimizations).

After working on [unfold](../unfold) I realized we can do more than doing very specialized stream fusion on an AST's leaf nodes.

The (hopeful) goal of this project is to use [Morte](https://github.com/Gabriel439/Haskell-Morte-Library) with Erlang as a frontend and Core Erlang as a backend.

This should enable one to get the compiler to optimize `g/0` to `h/0`:

<table>
<tr>
<td>
<pre lang="erlang">
-export([g/0]).

g () -> hd([get()]).
</pre>
</td>
<td>
<pre lang="erlang">
-export([h/0]).

h () -> get().
</pre>
</td>
<tr>
<td>
<pre lang="erlang">
{function, g, 0, 2}.
  {label,1}.
    {line,[{location,"t.erl",11}]}.
    {func_info,{atom,t},{atom,g},0}.
  {label,2}.
    {allocate,0,0}.
    {line,[{location,"t.erl",11}]}.
    {call_ext,0,{extfunc,erlang,get,0}}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,0}}.
    {line,[{location,"t.erl",11}]}.
    {bif,hd,{f,0},[{x,0}],{x,0}}.
    {deallocate,0}.
    return.
</pre>
</td>
<td>
<pre lang="erlang">
{function, h, 0, 4}.
  {label,3}.
    {line,[{location,"t.erl",12}]}.
    {func_info,{atom,t},{atom,h},0}.
  {label,4}.
    {call_ext_only,0,{extfunc,erlang,get,0}}.
</pre>
</td>
</tr>


I say hopeful because I have not verified whether the expressiveness of Erlang is enough in order for Morte to β-reduce and η-reduce expressions.
For starters: recursion in Erlang also allows for hot code swapping, however Morte unrolls such loops (supercompilers are slow enough as they are, no need for a compilation to outlive mankind).

A probably very naive alternative would be to
* leave out AST nodes with side effects (such as the expression `get()`)
* execute "clean" leaf ASTs up to a timeout, concurrently (see [`ct_expand`](https://github.com/uwiger/parse_trans/blob/master/src/ct_expand.erl))
* partially evaluate non leaf ASTs
