-module (event).
-compile(export_all).

-record(state, {server, name="", to_go=0}).

start(EventName, When) ->
    spawn(?MODULE, init, [self(), EventName, When]).

start_link(EventName, When) ->
    spawn_link(?MODULE, init, [self(), EventName, When]).

cancel(Pid) ->
    %%Monitor in case the process is already dead
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _reason} ->
            ok
    end.

%% Event's innards
init(Server, EventName, DateTime) ->
    loop(#state{server=Server,
                name=EventName,
                to_go=time_to_go(DateTime)}).

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

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut)
           - calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0 ->
                   ToGo;
              ToGo =< 0 -> 0
           end,
    normalize(Secs).

test() ->
    start("test", from_now(1))
        , timer:sleep(500)
        , flush()
        , timer:sleep(500)
        , flush()
        , Pid = start("test", from_now(500))
        , cancel(Pid)
        , Pid2 = start("test", from_now(365*24*60*60))
        , cancel(Pid2)
        .

from_now(Secs) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(calendar:local_time())
      + Secs).

flush() ->
    receive
        Foo -> io:format("~p got ~p~n", [self(), Foo]),
               flush()
    after 0 ->
            ok
    end.
