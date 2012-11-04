-module (event).
-compile(export_all).

-record(state, {server, name="", to_go=0}).

loop(S = #state{server=Server}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after S#state.to_go*1000 ->
            Server ! {done, S#state.name}
    end.
