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

%this wouild be Erlang's equivalent of a singleton pattern...
start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

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
            %%%io:format("monitoring ~p~n", [Client]),
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
        {Pid, MsgRef, {cancel, Name}} ->
            Events = case orddict:find(Name, S#state.events) of
                         {ok, E} ->
                             event:cancel(E#event.pid),
                             orddict:erase(Name, S#state.events);
                         error ->
                             S#state.events
                     end,
            Pid ! {MsgRef, ok},
            loop(S#state{events = Events});
        {done, Name} ->
            case orddict:find(Name, S#state.events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description}, S#state.clients),
                    NewEvents = orddict:erase(Name, S#state.events),
                    loop(S#state{events=NewEvents});
                error ->
                    %% this may happen if we cancel an event and it
                    %% fires at the same time
                    loop(S)
            end;
        shutdown ->
            exit(shutdown);
        {'DOWN', Ref, process, _Pid, _Reason} ->
            %%%io:format("removing monitor on ~p~n", [Ref]),
            loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
        code_change ->
            ?MODULE:loop(S);
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
    end.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

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
    Pid = start(),
    Client = spawn(?MODULE, simpleClient, [self()]),
    Ref = make_ref(),
    try
        %%subscribe
        Pid ! {self(), Ref, {subscribe, self()}},
        receive {Ref, ok} -> ok after 100 -> exit('timeout') end,

        %%subscribe a second client
        Pid ! {self(), Ref, {subscribe, Client}},
        receive {Ref, ok} -> ok after 100 -> exit('timeout') end,

        %%create two events
        Pid ! {self(), Ref, {add, "foo", "an event", event:from_now(1)}},
        receive {Ref, ok} -> ok after 100 -> exit('timeout') end,

        Pid ! {self(), Ref, {add, "bar", "another event", event:from_now(1)}},
        receive {Ref, ok} -> ok after 100 -> exit('timeout') end,

        %%cancel one of them
        Pid ! {self(), Ref, {cancel, "foo"}},
        receive {Ref, ok} -> ok after 100 -> exit('timeout') end,

        %%then we should receive from the active message
        receive
            {done, "bar", "another event"} -> ok
        after 1500 -> exit('timeout')
        end,

        %%as should the client, which we'll kill; it should be removed
        %%(there's a printf in evserv...)
        receive
            {clientReceived, {done, "bar", "another event"}} ->
                %%%io:format("killing ~p~n", [Client]),
                exit(Client, kill);
            Unexpected -> exit(io_lib:format('received an unexepcted message ~p',
                                             [Unexpected]))
        after 1500 -> exit('timeout')
        end,

        %%and nothing receives the cancelled message.
        receive Unexpected2 ->
                exit(io_lib:format('received an unexpected message ~p',
                                   [Unexpected2]))
        after 1500 -> ok
        end

    after
        exit(Client, kill),
        shutdown(),
        flush()
    end,
    ok.

simpleClient(Loopback) ->
    receive
        Message ->
            Loopback ! {clientReceived, Message},
            simpleClient(Loopback)
    end.



flush() ->
    receive
        Foo -> io:format("~p got ~p~n", [self(), Foo]),
               flush()
    after 0 ->
            ok
    end.
