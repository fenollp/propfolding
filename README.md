#propfolding • [GitHub](//github.com/fenollp/propfolding)

A parse transform that turns group calls to `lists:keyfind/3` and such into one-passes.

For example, this code running in `4·O(N)` (where `N = length(Article)`)

```erlang
meta (Article) ->
    {_, Title} = lists:keyfind(title, 1, Article),
    {_, Name}  = lists:keyfind(name, 1, Article),
    {_, Date}  = lists:keyfind(date, 1, Article),
    {_, Desc}  = lists:keyfind(desc, 1, Article),
    {Title, "/articles/" ++ Name, Date, Desc}.
```

will get rewritten into `1·O(N)`

```erlang
meta (Article) ->
    {Title, Name, Date, Desc} =
        lists:foldl(
          fun
              ({title, Title}, Acc) -> setelement(1, Acc, Title);
              ({name, Name}, Acc)   -> setelement(2, Acc, "/articles/"++Name);
              ({date, Date}, Acc)   -> setelement(3, Acc, Date);
              ({desc, Desc}, Acc)   -> setelement(4, Acc, Desc)
          end,
          {undef, undef, undef, undef},
         Article).
```

<!-- https://gist.github.com/fenollp/11190937 -->
<!-- ```erlang -->
<!-- get (Doc) -> -->
<!--     {Stars, AreaID, CheckinDate, HotelID, Rooms} = -->
<!--         lists:foldl( -->
<!--           fun -->
<!--               ({<<"stars">>, Stars},             Acc) -> setelement(1, Acc, Stars); -->
<!--               ({<<"areaID">>, AreaID},           Acc) -> setelement(2, Acc, AreaID); -->
<!--               ({<<"checkinDate">>, <<Year:4, $-, Month:2, $-, Day:2>>}, Acc) -> -->
<!--                   setelement(3, Acc, binary_to_integer(<<Year/binary, Month/binary, Day/binary>>)); -->
<!--               ({<<"hotelID">>, HotelID},         Acc) -> setelement(4, Acc, HotelID); -->
<!--               ({<<"rooms">>, Rooms},             Acc) -> setelement(5, Acc, Rooms) -->
<!--           end, -->
<!--           {undef, undef, undef, undef, undef}, -->
<!--           Doc), -->
<!--     %% blablabla -->
<!--     ok. -->
<!-- ``` -->


## To Do

* Preferably a core transform (does that exist?)
* Decide on what interface to provide
* Do we display a warning (“sub-optimal code detected”) or rewrite code?
