# unfold

[Ferd's answer to a *Pipe Operator in Erlang?* Erlang ML thread](http://erlang.org/pipermail/erlang-questions/2015-July/085109.html)

Turn

```erlang
f(X0) ->
    maybe_pipe(X0, [fun g/1
                   ,fun h/1]).

maybe_pipe(Init, Funs) ->
    %% use throws and catches if you wanna go faster
    lists:foldl(fun (F, {error, _R}=E) -> E;
                    (F, {ok, State}) -> F(State)
                end,
                Init,
                Funs).
```

into

```erlang
f(X0) ->
    case g(X0) of
        {error, _R}=E -> E;
        {ok, X1} ->
            case h(X1) of
                {error, _R}=E -> E;
                {ok, X2} -> {ok, X2}
            end
    end.
```
