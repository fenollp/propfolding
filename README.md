#propfolding • [GitHub](//github.com/fenollp/propfolding)

A parse transform that turns group calls to `lists:keyfind/3` and such into one-passes.

For example, this code running in `4·O(N)` (where `N = length(Article)`)

```erlang
meta (Article) ->
    Title = proplists:get_value(title, Article),
    Name  = proplists:get_value(name,  Article),
    Date  = proplists:get_value(date,  Article),
    Desc  = proplists:get_value(desc,  Article),
    {Title, "/articles/"++ Name, Date, Desc}.
```

will get rewritten into `1·O(N)`

```erlang
meta (Article) ->
    {Title, Name, Date, Desc} =
        lists:foldl(
          fun
              ({title, Title}, Acc) -> setelement(1, Acc, Title);
              ({name, Name}, Acc)   -> setelement(2, Acc, Name);
              ({date, Date}, Acc)   -> setelement(3, Acc, Date);
              ({desc, Desc}, Acc)   -> setelement(4, Acc, Desc)
          end
          , {undefined, undefined, undefined, undefined}
          , Article),
    {Title, "/articles/"++ Name, Date, Desc}.
```

using some transformation that could be described thusly:

```c
# Propfolding -- apply per scope (default applying behaviour since matching a var_id (here, List))
#define ( Varⁱ = proplists:get_value(Keyⁱ, List) ){2,ⁿ} (
    {(, Varⁱ)ⁿ} =
        lists:foldl(
          fun
      (       ({Keyⁱ, Varⁱ}, Acc⁰) -> setelement(#ⁱ, Acc⁰, Keyⁱ)       )ⁿ
          end
          , {(, undefined)ⁿ}
          , List )
  )
```


## To Do

* Preferably a core transform (does that exist?)
* Decide on what interface to provide
* Do we display a warning (“sub-optimal code detected”) or rewrite code?
