-module(evserv).
-compile(export_all).

-record(state, {  events %%list of #event{} records
                , clients %% list of Pids
                }).

-record(event, { name=""
                 , description=""
                 , pid
                 , timeout={{1970,1,1},{0,0,0}}
               }
       ).

init() ->
    %% Loading events from a static file could be done here.
    %% You would need to pass an argument to init telling where the
    %% resource to find the events is. Then load it from here.
    %% Another option is to just pass the events straight to the server
    %% through this function.
    loop(#state{events=orddict:new()
                , clients=orddict:new()
               }).

loop(S = #state{}) ->
    receive
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            NewClients = orddict:store(Ref, Client, S#state.clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients = NewClients});
        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            EventPid = event:start_link(Name, TimeOut),
            NewEvents = orddict:store(Name, #event{name=Name,
                                                   description=Description,
                                                   pid=EventPid,
                                                   timeout=TimeOut},
                                      S#state.events),
            Pid ! {MsgRef, ok},
            loop(S#state{events = NewEvents});
        %% {Pid, MsgRef, {cancel, Name}} ->
        %% {done, Name} ->
        %%     ...
        shutdown ->
            exit(shutdown);
        %% {'DOWN', Ref, process, _PID, _Reason} ->
        %%     ...
        %% code_change ->
        %%     ...
        %% Unknown ->
        %%     Io:format("Unknown message: ~pn", [Unknown]),
        %%     loop(State)
        _ -> nope
    end.

valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error_function_clause ->
            false
    end;
valid_datetime(_) ->
    false.

valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
                       M >= 0, M < 60,
                       S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.

test() ->
    case valid_datetime({{2012,12,31},{00,01,30}}) of true -> ok end,
    case valid_datetime({{2012,12,32},{00,01,30}}) of false -> ok end,
    case valid_time({23,59,59}) of true -> ok end,
    case valid_time({23,59,60}) of false -> ok end,
    Pid = spawn(?MODULE, init, []),
    Ref = make_ref(),
    Pid ! {self(), Ref, {add, "foo", "an event", event:from_now(1)}},
    receive {Ref, ok} -> ok after 500 -> exit('timeout') end,
    Pid ! {self(), Ref, {subscribe, self()}},
    receive {Ref, ok} -> ok after 500 -> exit('timeout') end,
    Pid ! shutdown,
    flush(),
    ok.

flush() ->
    receive
        Foo -> io:format("~p got ~p~n", [self(), Foo]),
               flush()
    after 0 ->
            ok
    end.
