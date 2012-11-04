-module (event).
-compile(export_all).

-record(state, {server, name="", to_go=0}).

loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T*1000 ->
              if Next =:= [] ->
                      Server ! {dont, S#state.name};
                 Next =/= [] ->
                      loop(S#state{to_go=Next})
              end
    end.

normalize(N) ->
    Limit = 49*24*60*60
        , [N rem Limit | lists:duplicate(N div Limit, Limit)].

test() ->
    spawn(event, loop, [#state{server=self(), name="test", to_go=normalize(1)}])
        , timer:sleep(500)
        , flush()
        , timer:sleep(1000)
        , flush()
        , Pid = spawn(event, loop,
                      [#state{server=self(), name="test", to_go=normalize(500)}])
        , ReplyRef = make_ref()
        , Pid ! {self(), ReplyRef, cancel}
        , flush()
        , Pid2 = spawn(event, loop, [#state{server=self(), name="test",
                                            to_go=normalize(365*24*60*60)}])
        , ReplyRef2 = make_ref()
        , Pid2 ! {self(), ReplyRef2, cancel}
        , flush()
        .

flush() ->
    receive
        Foo -> io:format("~p~n", [Foo]),
               flush()
    after 0 ->
            ok
    end.
